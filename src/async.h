
#ifndef R_ASYNC_H
#define R_ASYNC_H

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP async_poll(SEXP fds, SEXP timeout);

#endif
