
#include "async.h"

#include <errno.h>
#include <stdio.h>
#include <poll.h>

/* Various OSes and OS versions return various poll codes when the
   child's end of the pipe is closed, so we cannot provide a more
   elaborate API. See e.g. http://www.greenend.org.uk/rjk/tech/poll.html
   In particular, (recent) macOS return both POLLIN and POLLHUP,
   Cygwin return POLLHUP, and most others return just POLLIN, so there
   is not way to distinguish. Essentially, if a read would not block,
   and the fd is still open, then we return with PXREADY.

   So for us, we just have:
*/

int async__poll_decode(short code) {
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

SEXP async_poll(SEXP curl_fds, SEXP processx_handles, SEXP timeout) {
  int *c_curl_fds = INTEGER(curl_fds);
  int *c_px_fds = INTEGER(processx_handles);
  int c_timeout = INTEGER(timeout)[0];
  int num_curl_fds = LENGTH(curl_fds);
  int num_px_fds = LENGTH(processx_handles);
  int i, num_fds = num_curl_fds + num_px_fds;
  struct pollfd *pollfds;
  int ret;
  SEXP result = PROTECT(allocVector(INTSXP, num_fds));

  if (num_fds == 0) {
    UNPROTECT(1);
    return(result);
  }

  pollfds = (struct pollfd*) R_alloc(num_fds, sizeof(struct pollfd));
  for (i = 0; i < num_curl_fds; i++) {
    pollfds[i].fd = c_curl_fds[i];
    pollfds[i].events = POLLIN;
    pollfds[i].revents = 0;
  }
  for(i = num_curl_fds; i < num_fds; i++) {
    pollfds[i].fd = c_px_fds[i];
    pollfds[i].events = POLLIN;
    pollfds[i].revents = 0;
  }

  ret = async__interruptible_poll(pollfds, num_fds, c_timeout);

  if (ret == -1) {
    error("async poll error: %s", strerror(errno));
  }

  for (i = 0; i < num_fds; i++) {
    INTEGER(result)[i] = async__poll_decode(pollfds[i].revents);
  }

  UNPROTECT(1);
  return result;
}
