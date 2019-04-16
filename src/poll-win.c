
#include "async.h"
#include "winsock2.h"

int async__interruptible_poll(int num_fds, fd_set *readfds, int timeout) {
  int ret = 0;
  int timeleft = timeout;
  TIMEVAL w_timeout;

  w_timeout.tv_sec = ASYNC_INTERRUPT_INTERVAL / 1000;
  w_timeout.tv_usec = ASYNC_INTERRUPT_INTERVAL % 1000 * 1000;

  while (timeout < 0 || timeleft > ASYNC_INTERRUPT_INTERVAL) {
    ret = select(num_fds, readfds, NULL, NULL, &w_timeout);

    if (ret != 0) return ret;

    R_CheckUserInterrupt();
    timeleft -= ASYNC_INTERRUPT_INTERVAL;
  }

  /* Maybe there is a little bit more time left */
  if (timeleft >= 0) {
    w_timeout.tv_sec = timeleft / 1000;
    w_timeout.tv_usec = timeleft % 1000 * 1000;
    ret = select(num_fds, readfds, NULL, NULL, &w_timeout);
  }

  return ret;
}

SEXP async_poll(SEXP curl_fds, SEXP processx_handles, SEXP timeout) {
  int *c_fds = INTEGER(curl_fds);
  int c_timeout = INTEGER(timeout)[0];
  int i, num_fds = LENGTH(curl_fds);
  SEXP result = PROTECT(allocVector(INTSXP, num_fds));
  int ret = 0;
  fd_set readfds;

  FD_ZERO(&readfds);
  for (i = 0; i < num_fds; i++) {
    FD_SET(c_fds[i], &readfds);
  }

  if (num_fds > 0) {
    async__interruptible_poll(num_fds, &readfds, c_timeout);
  }

  if (ret == SOCKET_ERROR) {
    /* TODO: proper error message */
    error("async poll error: %d", WSAGetLastError());
  }

  if (ret == 0) {
    for (i = 0; i < num_fds; i++) {
      INTEGER(result)[i] = PXTIMEOUT;
    }

  } else {
    for (i = 0; i < num_fds; i++) {
      if (FD_ISSET(c_fds[i], &readfds)) {
	INTEGER(result)[i] = PXREADY;
      } else {
	INTEGER(result)[i] = PXSILENT;
      }
    }
  }

  UNPROTECT(1);
  return result;
}
