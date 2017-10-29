
#' @export

print.async_deferred_rejected <- function(x, ...) {

  call <- x$call

  if (h <- x$hide[1,1] %||% 0) call$start <- tail(call$start, -h)
  if (h <- x$hide[1,2] %||% 0) call$start <- head(call$start, -h)
  if (h <- x$hide[2,1] %||% 0) call$eval <- tail(call$eval, -h)
  if (h <- x$hide[2,2] %||% 0) call$eval <- head(call$eval, -h)

  cat("ERROR:\n")
  cat(paste(" ", x$message), sep = "\n")
  if (!is.null(x$parent)) print(x$parent)
  cat("START:\n")
  cat(paste(" ", seq_along(call$start), as.character(call$start)),
      sep = "\n")
  cat("EVAL:\n")
  cat(paste(" ", seq_along(call$eval), as.character(call$eval)),
      sep = "\n")

  invisible(x)
}
