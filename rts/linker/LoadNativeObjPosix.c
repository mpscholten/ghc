#include "LinkerInternals.h"
#include "Rts.h"
#include "sm/OSMem.h"

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

#include "CheckUnload.h"
#include "ForeignExports.h"
#include "RtsUtils.h"
#include "Profiling.h"

#include "linker/LoadNativeObjPosix.h"

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(HAVE_DLINFO)
#include <link.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

/*
 * Shared object loading
 */

#if defined(HAVE_DLINFO)
struct piterate_cb_info {
  ObjectCode *nc;
  void *l_addr;   /* base virtual address of the loaded code */
};

static int loadNativeObjCb_(struct dl_phdr_info *info,
    size_t _size STG_UNUSED, void *data) {
  struct piterate_cb_info *s = (struct piterate_cb_info *) data;

  // This logic mimicks _dl_addr_inside_object from glibc
  // For reference:
  // int
  // internal_function
  // _dl_addr_inside_object (struct link_map *l, const ElfW(Addr) addr)
  // {
  //   int n = l->l_phnum;
  //   const ElfW(Addr) reladdr = addr - l->l_addr;
  //
  //   while (--n >= 0)
  //     if (l->l_phdr[n].p_type == PT_LOAD
  //         && reladdr - l->l_phdr[n].p_vaddr >= 0
  //         && reladdr - l->l_phdr[n].p_vaddr < l->l_phdr[n].p_memsz)
  //       return 1;
  //   return 0;
  // }

  if ((void*) info->dlpi_addr == s->l_addr) {
    int n = info->dlpi_phnum;
    while (--n >= 0) {
      if (info->dlpi_phdr[n].p_type == PT_LOAD) {
        NativeCodeRange* ncr =
          stgMallocBytes(sizeof(NativeCodeRange), "loadNativeObjCb_");
        ncr->start = (void*) ((char*) s->l_addr + info->dlpi_phdr[n].p_vaddr);
        ncr->end = (void*) ((char*) ncr->start + info->dlpi_phdr[n].p_memsz);

        ncr->next = s->nc->nc_ranges;
        s->nc->nc_ranges = ncr;
      }
    }
  }
  return 0;
}
#endif /* defined(HAVE_DLINFO) */

static void copyErrmsg(char** errmsg_dest, char* errmsg) {
  if (errmsg == NULL) errmsg = "loadNativeObj_POSIX: unknown error";
  *errmsg_dest = stgMallocBytes(strlen(errmsg)+1, "loadNativeObj_POSIX");
  strcpy(*errmsg_dest, errmsg);
}

void freeNativeCode_POSIX (ObjectCode *nc) {
  ASSERT_LOCK_HELD(&linker_mutex);

  dlclose(nc->dlopen_handle);

  NativeCodeRange *ncr = nc->nc_ranges;
  while (ncr) {
    NativeCodeRange* last_ncr = ncr;
    ncr = ncr->next;
    stgFree(last_ncr);
  }
}

/*
 * Note [Don't fail due to RTLD_NOW]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * If possible we want to load dynamic objects immediately (e.g. using
 * RTLD_NOW) so that we can query their mappings and therefore be able to
 * safely unload them. However, there are some cases where an object cannot be
 * successfully eagerly loaded yet execution can nevertheless succeed with lazy
 * binding.
 *
 * One such instance was found in #25943, where a library referenced undefined
 * symbols. While this pattern is quite dodgy (really, these symbol references
 * should be weakly bound in the library), previous GHC versions accepted such
 * programs. Moreover, it is important that we are able to load such libraries
 * since GHC insists on loading all package dependencies when, e.g., evaluating
 * TemplateHaskell splices.
 *
 * To ensure that we don't fail to load such programs, we first attempt loading
 * with RTLD_NOW and, if this fails, attempt to load again with lazy binding
 * (taking care to mark the object as not unloadable in this case).
 */

/*
 * Note [Two-phase loading for concurrent dlopen]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * To enable concurrent loading of dynamic libraries, we split the loading
 * process into two phases:
 *
 * Phase 1 (loadNativeObj_POSIX_phase1):
 *   - Creates the ObjectCode structure
 *   - Calls dlopen() to load the library
 *   - Processes foreign exports via the TLS loading_obj mechanism
 *   - Does NOT require the global linker_mutex
 *   - Can run in parallel with other phase1 calls
 *
 * Phase 2 (loadNativeObj_POSIX_phase2):
 *   - Registers the ObjectCode with global linker state
 *   - Updates loaded_objects list
 *   - Calls insertOCSectionIndices
 *   - REQUIRES the global linker_mutex
 *   - Must run sequentially
 *
 * The original loadNativeObj_POSIX function calls both phases sequentially
 * while holding the linker_mutex throughout (for backward compatibility).
 *
 * For parallel loading, use loadNativeObjBatch_POSIX which:
 *   1. Runs phase1 for all libraries in parallel (using OS threads)
 *   2. Runs phase2 for all libraries sequentially under linker_mutex
 *
 * This approach allows the expensive dlopen() system calls to happen in
 * parallel, while maintaining the safety of the global linker state.
 */

// The following types and functions are only used for parallel loading in THREADED_RTS
#if defined(THREADED_RTS)

// Result of phase 1 loading
typedef struct {
    ObjectCode *oc;      // The created ObjectCode (NULL on failure)
    void *dlopen_handle; // The dlopen handle (may be NULL if oc owns it)
    char *errmsg;        // Error message on failure (NULL on success)
    bool success;        // Whether phase 1 succeeded
} LoadNativeObjPhase1Result;

// Phase 1: Load the library without holding linker_mutex
// This can be called concurrently from multiple threads
static LoadNativeObjPhase1Result
loadNativeObj_POSIX_phase1(pathchar *path, bool check_already_loaded)
{
    LoadNativeObjPhase1Result result = {
        .oc = NULL,
        .dlopen_handle = NULL,
        .errmsg = NULL,
        .success = false
    };

    IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX_phase1 %" PATH_FMT "\n", path));

    // Check if already loaded - this requires linker_mutex briefly
    // But we only do a read, and the worst case is a redundant load attempt
    if (check_already_loaded) {
        ObjectCode *existing_oc = lookupObjectByPath(path);
        if (existing_oc && existing_oc->status != OBJECT_UNLOADED) {
            if (existing_oc->type == DYNAMIC_OBJECT) {
                // Already loaded, return success with the existing handle
                result.dlopen_handle = existing_oc->dlopen_handle;
                result.success = true;
                return result;
            }
            copyErrmsg(&result.errmsg, "loadNativeObj_POSIX: already loaded as non-dynamic object");
            return result;
        }
    }

    ObjectCode *nc = mkOc(DYNAMIC_OBJECT, path, NULL, 0, false, NULL, 0);

    // Determine whether to use RTLD_NOW or RTLD_LAZY
    bool load_now;
#if defined(HAVE_DLINFO)
    load_now = true;
#else
    load_now = false;
#endif

    void *hdl = NULL;

try_again:
    // Set thread-local loading_obj for foreign exports registration
    // See Note [Thread-local loading_obj for concurrent dlopen] in ForeignExports.c
    foreignExportsLoadingObject(nc);

#if defined(PROFILING)
    ACQUIRE_LOCK(&ccs_mutex);
#endif

    const int dlopen_mode = load_now ? RTLD_NOW : RTLD_LAZY;
    hdl = dlopen(path, dlopen_mode | RTLD_LOCAL);
    nc->dlopen_handle = hdl;
    nc->status = OBJECT_READY;

#if defined(PROFILING)
    RELEASE_LOCK(&ccs_mutex);
#endif

    // Use deferred version - processForeignExports will be called in phase2
    // under linker_mutex. See Note [Two-phase loading for concurrent dlopen].
    foreignExportsFinishedLoadingObject_deferred();

    if (hdl == NULL) {
        if (load_now) {
            // See Note [Don't fail due to RTLD_NOW]
            load_now = false;
            goto try_again;
        } else {
            copyErrmsg(&result.errmsg, dlerror());
            goto fail;
        }
    }

#if defined(HAVE_DLINFO)
    if (load_now) {
        struct link_map *map;
        if (dlinfo(hdl, RTLD_DI_LINKMAP, &map) == -1) {
            copyErrmsg(&result.errmsg, dlerror());
            goto fail_with_close;
        }

        struct piterate_cb_info piterate_info = {
            .nc = nc,
            .l_addr = (void *) map->l_addr
        };
        dl_iterate_phdr(loadNativeObjCb_, &piterate_info);
        if (!nc->nc_ranges) {
            copyErrmsg(&result.errmsg, "dl_iterate_phdr failed to find obj");
            goto fail_with_close;
        }
        nc->unloadable = true;
    } else {
        nc->nc_ranges = NULL;
        nc->unloadable = false;
    }
#else
    nc->nc_ranges = NULL;
    nc->unloadable = false;
#endif

    // Success - phase 1 complete
    result.oc = nc;
    result.dlopen_handle = nc->dlopen_handle;
    result.success = true;
    return result;

#if defined(HAVE_DLINFO)
fail_with_close:
    if (hdl) dlclose(hdl);
#endif
fail:
    if (nc) {
        // Clean up the ObjectCode but not the dlopen handle
        // (which may have already been closed or was never opened)
        nc->dlopen_handle = NULL;
        freeObjectCode(nc);
    }
    return result;
}

// Phase 2: Register the ObjectCode with global linker state
// This MUST be called while holding linker_mutex
// If process_foreign_exports is false, the caller must call processForeignExports later
static void *
loadNativeObj_POSIX_phase2_internal(LoadNativeObjPhase1Result *phase1_result,
                                    bool process_foreign_exports)
{
    ASSERT_LOCK_HELD(&linker_mutex);

    if (!phase1_result->success || phase1_result->oc == NULL) {
        // Phase 1 failed or returned an already-loaded handle
        return phase1_result->dlopen_handle;
    }

    ObjectCode *nc = phase1_result->oc;

    IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX_phase2 %" PATH_FMT "\n", nc->fileName));

    // Register section indices for GC
    insertOCSectionIndices(nc);

    // Add to loaded_objects list
    nc->next_loaded_object = loaded_objects;
    loaded_objects = nc;

    // Process foreign exports that were registered during phase1's dlopen.
    // This must happen under linker_mutex because it modifies oc->foreign_exports.
    // For batch loading, we defer this to process all at once for efficiency.
    if (process_foreign_exports) {
        processForeignExports();
    }

#if defined(PROFILING)
    // Collect any new cost centres defined in the loaded object
    refreshProfilingCCSs();
#endif

    IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX_phase2 result=%p\n", nc->dlopen_handle));

    return nc->dlopen_handle;
}

#include <pthread.h>

// Structure for passing data to worker threads
typedef struct {
    pathchar *path;
    LoadNativeObjPhase1Result result;
} LoadNativeObjWorkItem;

static void *loadNativeObj_worker(void *arg) {
    LoadNativeObjWorkItem *item = (LoadNativeObjWorkItem *)arg;
    item->result = loadNativeObj_POSIX_phase1(item->path, true);
    return NULL;
}

// Batch load multiple native objects in parallel
// Returns array of handles on success, NULL on first failure (with errmsg set)
// The caller must free the returned array
void ** loadNativeObjBatch_POSIX(pathchar **paths, int n_paths, char **errmsg)
{
    IF_DEBUG(linker, debugBelch("loadNativeObjBatch_POSIX: loading %d libraries in parallel\n", n_paths));
    IF_DEBUG(linker, for (int i = 0; i < n_paths; i++) {
        debugBelch("  [%d] %" PATH_FMT "\n", i, paths[i]);
    });

    // Check if sequential loading is forced via environment variable (for benchmarking)
    bool force_sequential = getenv("GHC_LOADER_SEQUENTIAL") != NULL;

    if (n_paths > 1) {
        if (force_sequential) {
            struct timeval start, end;
            gettimeofday(&start, NULL);
            fprintf(stderr, "[sequential-loader] Loading %d libraries sequentially\n", n_paths);
            // Fall through to sequential loading below
            void **handles = stgMallocBytes(n_paths * sizeof(void *), "loadNativeObjBatch_POSIX handles");
            for (int i = 0; i < n_paths; i++) {
                ACQUIRE_LOCK(&linker_mutex);
                handles[i] = loadNativeObj_POSIX(paths[i], errmsg);
                RELEASE_LOCK(&linker_mutex);
                if (handles[i] == NULL) {
                    stgFree(handles);
                    return NULL;
                }
            }
            gettimeofday(&end, NULL);
            long elapsed_ms = (end.tv_sec - start.tv_sec) * 1000 + (end.tv_usec - start.tv_usec) / 1000;
            fprintf(stderr, "[sequential-loader] Finished in %ld ms\n", elapsed_ms);
            return handles;
        } else {
            fprintf(stderr, "[parallel-loader] Loading %d libraries using %d threads\n", n_paths, n_paths);
        }
    }

    if (n_paths == 0) {
        return stgMallocBytes(0, "loadNativeObjBatch_POSIX");
    }

    // Start timing for parallel loading
    struct timeval start;
    gettimeofday(&start, NULL);

    // Allocate work items and result array
    LoadNativeObjWorkItem *work_items = stgMallocBytes(
        n_paths * sizeof(LoadNativeObjWorkItem), "loadNativeObjBatch_POSIX work_items");
    pthread_t *threads = stgMallocBytes(
        n_paths * sizeof(pthread_t), "loadNativeObjBatch_POSIX threads");
    void **handles = stgMallocBytes(
        n_paths * sizeof(void *), "loadNativeObjBatch_POSIX handles");

    // Initialize work items
    for (int i = 0; i < n_paths; i++) {
        work_items[i].path = paths[i];
        work_items[i].result.oc = NULL;
        work_items[i].result.dlopen_handle = NULL;
        work_items[i].result.errmsg = NULL;
        work_items[i].result.success = false;
    }

    // Phase 1: Spawn threads to load libraries in parallel
    // Note: We don't hold linker_mutex during this phase
    for (int i = 0; i < n_paths; i++) {
        int rc = pthread_create(&threads[i], NULL, loadNativeObj_worker, &work_items[i]);
        if (rc != 0) {
            // Failed to create thread - fall back to sequential loading for remaining
            IF_DEBUG(linker, debugBelch("loadNativeObjBatch_POSIX: pthread_create failed, falling back to sequential\n"));
            work_items[i].result = loadNativeObj_POSIX_phase1(work_items[i].path, true);
            // Mark remaining threads as not started
            for (int j = i + 1; j < n_paths; j++) {
                threads[j] = 0;
            }
            break;
        }
    }

    // Wait for all threads to complete
    for (int i = 0; i < n_paths; i++) {
        if (threads[i] != 0) {
            pthread_join(threads[i], NULL);
        } else if (!work_items[i].result.success && work_items[i].result.errmsg == NULL) {
            // Thread wasn't started, run sequentially
            work_items[i].result = loadNativeObj_POSIX_phase1(work_items[i].path, true);
        }
    }

    // Check for any failures in phase 1
    for (int i = 0; i < n_paths; i++) {
        if (!work_items[i].result.success) {
            // Copy the error message
            if (work_items[i].result.errmsg) {
                *errmsg = work_items[i].result.errmsg;
            } else {
                *errmsg = stgMallocBytes(64, "loadNativeObjBatch_POSIX errmsg");
                snprintf(*errmsg, 64, "loadNativeObjBatch_POSIX: failed to load library %d", i);
            }

            // Clean up all results
            for (int j = 0; j < n_paths; j++) {
                if (j != i && work_items[j].result.errmsg) {
                    stgFree(work_items[j].result.errmsg);
                }
                // Note: We don't close dlopen handles here because some may have succeeded
                // The caller should handle cleanup
            }
            stgFree(work_items);
            stgFree(threads);
            stgFree(handles);
            return NULL;
        }
    }

    // Phase 2: Register all ObjectCodes with global state (sequentially, under lock)
    ACQUIRE_LOCK(&linker_mutex);
    for (int i = 0; i < n_paths; i++) {
        // Don't process foreign exports yet - we'll do it once at the end
        handles[i] = loadNativeObj_POSIX_phase2_internal(&work_items[i].result, false);
    }
    // Process all foreign exports at once for efficiency
    processForeignExports();
    RELEASE_LOCK(&linker_mutex);

    struct timeval end;
    gettimeofday(&end, NULL);
    long elapsed_ms = (end.tv_sec - start.tv_sec) * 1000 + (end.tv_usec - start.tv_usec) / 1000;
    fprintf(stderr, "[parallel-loader] Finished in %ld ms\n", elapsed_ms);

    IF_DEBUG(linker, debugBelch("loadNativeObjBatch_POSIX: done loading %d libraries\n", n_paths));

    stgFree(work_items);
    stgFree(threads);
    return handles;
}

#else /* !THREADED_RTS */

// Non-threaded RTS: just load sequentially
void ** loadNativeObjBatch_POSIX(pathchar **paths, int n_paths, char **errmsg)
{
    void **handles = stgMallocBytes(n_paths * sizeof(void *), "loadNativeObjBatch_POSIX handles");

    for (int i = 0; i < n_paths; i++) {
        handles[i] = loadNativeObj_POSIX(paths[i], errmsg);
        if (handles[i] == NULL) {
            // Clean up and return failure
            stgFree(handles);
            return NULL;
        }
    }

    return handles;
}

#endif /* THREADED_RTS */

void * loadNativeObj_POSIX (pathchar *path, char **errmsg)
{
   ObjectCode* nc;
   void *hdl, *retval;

   ASSERT_LOCK_HELD(&linker_mutex);

   IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX %" PATH_FMT "\n", path));

   retval = NULL;

   /* If we load the same object multiple times, just return the
    * already-loaded handle. Note that this is broken if unloadNativeObj
    * is used, as we don’t do any reference counting; see #24345.
    */
   ObjectCode *existing_oc = lookupObjectByPath(path);
   if (existing_oc && existing_oc->status != OBJECT_UNLOADED) {
     if (existing_oc->type == DYNAMIC_OBJECT) {
       retval = existing_oc->dlopen_handle;
       goto success;
     }
     copyErrmsg(errmsg, "loadNativeObj_POSIX: already loaded as non-dynamic object");
     goto dlopen_fail;
   }

   nc = mkOc(DYNAMIC_OBJECT, path, NULL, 0, false, NULL, 0);

   // If we HAVE_DLINFO, we use RTLD_NOW rather than RTLD_LAZY because we want
   // to learn eagerly about all external functions. Otherwise, there is no
   // additional advantage to being eager, so it is better to be lazy and only
   // bind functions when needed for better performance.
   //
   // Moreover, it is possible that loading will fail (e.g. if the library
   // being loaded depends upon symbols from a library which is not available);
   // in this case we will retry loading with load_now=false. See
   // Note [Don't fail due to RTLD_NOW]..
   bool load_now;
#if defined(HAVE_DLINFO)
   load_now = true;
#else
   load_now = false;
#endif

try_again:
   foreignExportsLoadingObject(nc);

   // When dlopen() loads a profiled dynamic library, it calls the ctors which
   // will call registerCcsList() to append the defined CostCentreStacks to
   // CCS_LIST. However, another thread may be doing other things with the RTS
   // linker that transitively calls refreshProfilingCCSs() which also accesses
   // CCS_LIST. So there's a risk of data race that may lead to segfaults
   // (#24423), and we need to ensure the ctors are also protected by
   // ccs_mutex.
#if defined(PROFILING)
   ACQUIRE_LOCK(&ccs_mutex);
#endif

   const int dlopen_mode = load_now ? RTLD_NOW : RTLD_LAZY;
   hdl = dlopen(path, dlopen_mode|RTLD_LOCAL); /* see Note [RTLD_LOCAL] */
   nc->dlopen_handle = hdl;
   nc->status = OBJECT_READY;

#if defined(PROFILING)
   RELEASE_LOCK(&ccs_mutex);
#endif

   foreignExportsFinishedLoadingObject();

   if (hdl == NULL) {
     if (load_now) {
       // See Note [Don't fail due to RTLD_NOW]
       load_now = false;
       goto try_again;
     } else {
       /* dlopen failed; save the message in errmsg */
       copyErrmsg(errmsg, dlerror());
       goto dlopen_fail;
     }
   }

#if defined(HAVE_DLINFO)
   if (load_now) {
     struct link_map *map;
     if (dlinfo(hdl, RTLD_DI_LINKMAP, &map) == -1) {
       /* dlinfo failed; save the message in errmsg */
       copyErrmsg(errmsg, dlerror());
       goto dlinfo_fail;
     }

     hdl = NULL; // pass handle ownership to nc

     struct piterate_cb_info piterate_info = {
       .nc = nc,
       .l_addr = (void *) map->l_addr
     };
     dl_iterate_phdr(loadNativeObjCb_, &piterate_info);
     if (!nc->nc_ranges) {
       copyErrmsg(errmsg, "dl_iterate_phdr failed to find obj");
       goto dl_iterate_phdr_fail;
     }
     nc->unloadable = true;
   } else {
     nc->nc_ranges = NULL;
     nc->unloadable = false;
   }
#else
   nc->nc_ranges = NULL;
   nc->unloadable = false;
#endif /* defined (HAVE_DLINFO) */

   insertOCSectionIndices(nc);

   nc->next_loaded_object = loaded_objects;
   loaded_objects = nc;

   retval = nc->dlopen_handle;

#if defined(PROFILING)
  // collect any new cost centres that were defined in the loaded object.
  refreshProfilingCCSs();
#endif

   goto success;

#if defined(HAVE_DLINFO)
dl_iterate_phdr_fail:
#endif
   freeNativeCode_POSIX(nc);
#if defined(HAVE_DLINFO)
dlinfo_fail:
#endif
   if (hdl) dlclose(hdl);
dlopen_fail:
success:

   IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX result=%p\n", retval));

   return retval;
}

#endif /* elf + macho */
