#pragma once

#include "Rts.h"
#include "LinkerInternals.h"

#include "BeginPrivate.h"

void freeNativeCode_POSIX  ( ObjectCode *nc );
void *loadNativeObj_POSIX  ( pathchar *path, char **errmsg );

/*
 * Batch load multiple native objects in parallel.
 * See Note [Two-phase loading for concurrent dlopen] in LoadNativeObjPosix.c
 *
 * Returns array of handles on success, NULL on first failure (with errmsg set).
 * The caller must free the returned array using stgFree().
 */
void **loadNativeObjBatch_POSIX ( pathchar **paths, int n_paths, char **errmsg );

#include "EndPrivate.h"
