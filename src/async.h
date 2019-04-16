
#ifndef R_ASYNC_H
#define R_ASYNC_H

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#define PXNOPIPE  1		/* we never captured this output */
#define PXREADY   2		/* one fd is ready, or got EOF */
#define PXTIMEOUT 3		/* no fd is ready before the timeout */
#define PXCLOSED  4		/* fd was already closed when started polling */
#define PXSILENT  5		/* still open, but no data or EOF for now. */
                                /* No timeout, either, but events on other fds */

#define ASYNC_INTERRUPT_INTERVAL 100

SEXP async_poll(SEXP curl_fds, SEXP processx_handles, SEXP timeout);

#endif
