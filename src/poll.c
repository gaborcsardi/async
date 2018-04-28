
#include "async.h"

#include <errno.h>
#include <stdio.h>
#include <poll.h>

#define ASYNC_INTERRUPT_INTERVAL 200

/* Various OSes and OS versions return various poll codes when the
   child's end of the pipe is closed, so we cannot provide a more
   elaborate API. See e.g. http://www.greenend.org.uk/rjk/tech/poll.html
   In particular, (recent) macOS return both POLLIN and POLLHUP,
   Cygwin return POLLHUP, and most others return just POLLIN, so there
   is not way to distinguish. Essentially, if a read would not block,
   and the fd is still open, then we return with PXREADY.

   So for us, we just have:
*/

#define PXNOPIPE  1		/* we never captured this output */
#define PXREADY   2		/* one fd is ready, or got EOF */
#define PXTIMEOUT 3		/* no fd is ready before the timeout */
#define PXCLOSED  4		/* fd was already closed when started polling */
#define PXSILENT  5		/* still open, but no data or EOF for now. No timeout, either */
                                /* but there were events on other fds */

int processx__poll_decode(short code) {
  if (code & POLLNVAL) return PXCLOSED;
  if (code & POLLIN || code & POLLHUP) return PXREADY;
  return PXSILENT;
}

int async__interruptible_poll(struct pollfd fds[],
			      nfds_t nfds, int timeout) {
  int ret = 0;
  int timeleft = timeout;

  while (timeout < 0 || timeleft > ASYNC_INTERRUPT_INTERVAL) {
    do {
      ret = poll(fds, nfds, ASYNC_INTERRUPT_INTERVAL);
    } while (ret == -1 && errno == EINTR);

    /* If not a timeout, then return */
    if (ret != 0) return ret;

    R_CheckUserInterrupt();
    timeleft -= ASYNC_INTERRUPT_INTERVAL;
  }

  /* Maybe we are not done, and there is a little left from the timeout */
  if (timeleft >= 0) {
    do {
      ret = poll(fds, nfds, timeleft);
    } while (ret == -1 && errno == EINTR);
  }

  return ret;
}

SEXP async_poll(SEXP fds, SEXP timeout) {
  int *c_fds = INTEGER(fds);
  int c_timeout = INTEGER(timeout)[0];
  int i, num_fds = LENGTH(fds);
  struct pollfd *pollfds;
  int ret;

  if (num_fds == 0) return(ScalarLogical(0));

  pollfds = (struct pollfd*) R_alloc(num_fds, sizeof(struct pollfd));
  for (i = 0; i < num_fds; i++) {
    pollfds[i].fd = c_fds[i];
    pollfds[i].events = POLLIN;
    pollfds[i].revents = 0;
  }

  ret = async__interruptible_poll(pollfds, num_fds, c_timeout);

  if (ret == -1) {
    error("async poll error: %s", strerror(errno));

  }

  return(ScalarLogical(ret != 0));
}
