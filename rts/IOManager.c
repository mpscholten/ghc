/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Hooks for the I/O subsystem(s) that are called from other parts of the RTS.
 *
 * There are several different I/O subsystem implementations (aka I/O managers),
 * for different platforms (notably Windows vs others), and for the threaded vs
 * non-threaded RTS. These implementations all need hooks into other parts of
 * the RTS, such as startup/shutdown, the scheduler and other special features.
 *
 * To keep things comprehensible, all the hooks used by all the different I/O
 * subsystem implementations are centralised here. Not all implementations use
 * all hooks.
 *
 * -------------------------------------------------------------------------*/

#include "Rts.h"
#include "rts/IOInterface.h" // exported
#include "IOManager.h"       // RTS internal
#include "Capability.h"
#include "Schedule.h"
#include "RtsFlags.h"
#include "RtsUtils.h"

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)
#include "posix/Signals.h"
#endif

#if defined(mingw32_HOST_OS)
#include "win32/ThrIOManager.h"
#include "win32/AsyncMIO.h"
#include "win32/AsyncWinIO.h"
#endif


/* Allocate and initialise the per-capability CapIOManager that lives in each
 * Capability. Called early in the RTS initialisation.
 */
void initCapabilityIOManager(CapIOManager **piomgr)
{
    CapIOManager *iomgr =
      (CapIOManager *) stgMallocBytes(sizeof(CapIOManager),
                                      "initCapabilityIOManager");

#if defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
    iomgr->control_fd = -1;
#endif

    *piomgr = iomgr;
}


/* Called late in the RTS initialisation
 */
void
initIOManager(void)
{

#if defined(THREADED_RTS)
    /* Posix implementation in posix/Signals.c
     * Win32 implementation in win32/ThrIOManager.c
     */
    ioManagerStart();

#elif defined(mingw32_HOST_OS)
    /* Non-threaded Win32 implementation, either the WinIO IOCP implementation,
     * or the classic implementation. */
    if (is_io_mng_native_p()) {
        startupAsyncWinIO();
    } else {
        startupAsyncIO();
    }

#else
    /* The other implementation is the non-threaded Posix select() one.
     * It does not need any initialisation.
     */
#endif

}

/* Called from forkProcess in the child process on the surviving capability.
 */
void
initIOManagerAfterFork(Capability **pcap USED_IF_THREADS_AND_NOT_MINGW32)
{
#if defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
    /* Posix implementation in posix/Signals.c
     *
     * No non-threaded impl because the non-threaded Posix select() I/O manager
     * does not need any initialisation.
     *
     * No Windows impl since no forking.
     *
     * TODO: figure out if it is necessary for the threaded MIO case for the
     * starting of the IO manager threads to be synchronous. It would be
     * simpler if it could start them asynchronously and thus not have to
     * have the pcap as an inout param, that could be modified. In practice it
     * cannot be modified anyway since in the contexts where it is called
     * (forkProcess), there is only a single cap available.
     */
    ioManagerStartCap(pcap);
#endif
}


/* Called in the RTS shutdown before the scheduler exits
 */
void
stopIOManager(void)
{

#if defined(THREADED_RTS)
    /* Posix implementation in posix/Signals.c
     * Win32 implementation in win32/ThrIOManager.c
     */
    ioManagerDie();
#endif

}


/* Called in the RTS shutdown after the scheduler exits
 */
void
exitIOManager(bool wait_threads USED_IF_NOT_THREADS_AND_MINGW32)
{

#if !defined(THREADED_RTS) && defined(mingw32_HOST_OS)
    if (is_io_mng_native_p()) {
        shutdownAsyncWinIO(wait_threads);
    } else {
        shutdownAsyncIO(wait_threads);
    }
#endif

}

/* Wakeup hook: called from the scheduler's wakeUpRts (currently only in
 * threaded mode).
 */
void wakeupIOManager(void)
{
#if defined(THREADED_RTS)
    /* Posix implementation in posix/Signals.c
     * Win32 implementation in win32/ThrIOManager.c
     */
    ioManagerWakeup();
#endif
}


void markCapabilityIOManager(evac_fn evac STG_UNUSED,
                             void *user STG_UNUSED,
                             CapIOManager *iomgr STG_UNUSED)
{
}


/* Declared in rts/IOInterface.h. Used only by the MIO threaded I/O manager on
 * Unix platforms.
 */
#if !defined(mingw32_HOST_OS)
void
setIOManagerControlFd(uint32_t cap_no USED_IF_THREADS, int fd USED_IF_THREADS) {
#if defined(THREADED_RTS)
    if (cap_no < n_capabilities) {
        RELAXED_STORE(&capabilities[cap_no]->iomgr->control_fd, fd);
    } else {
        errorBelch("warning: setIOManagerControlFd called with illegal capability number.");
    }
#endif
}
#endif

#if !defined(THREADED_RTS)
void appendToIOBlockedQueue(StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (blocked_queue_hd == END_TSO_QUEUE) {
        blocked_queue_hd = tso;
    } else {
        setTSOLink(&MainCapability, blocked_queue_tl, tso);
    }
    blocked_queue_tl = tso;
}

void insertIntoSleepingQueue(StgTSO *tso, LowResTime target)
{
    StgTSO *prev = NULL;
    StgTSO *t = sleeping_queue;
    while (t != END_TSO_QUEUE && t->block_info.target < target) {
        prev = t;
        t = t->_link;
    }

    tso->_link = t;
    if (prev == NULL) {
        sleeping_queue = tso;
    } else {
        setTSOLink(&MainCapability, prev, tso);
    }
}
#endif
