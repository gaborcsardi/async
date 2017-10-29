
#' @export

print.async_deferred_rejected <- function(x, ...) {

  cat("Error:", x$message, "\n\n")
  print_long_stack(x$call)
  invisible(x)
}

print_long_stack <- function(call) {

  separator <- function() cat("  --------\n")

  if (!is.null(call$parent)) {
    print_long_stack(call$parent)
    separator()
  }

  call <- trim_long_stack(call)

  cat(paste(" ", seq_along(call$start), as.character(call$start)),
      sep = "\n")
  if (length(call$eval)) {
    separator()
    cat(paste(" ", seq_along(call$eval), as.character(call$eval)),
        sep = "\n")
  }
}


trim_long_stack <- function(call) {
  if (h <- call$hide[1,1] %||% 0) call$start <- tail(call$start, -h)
  if (h <- call$hide[1,2] %||% 0) call$start <- head(call$start, -h)
  if (h <- call$hide[2,1] %||% 0) call$eval <- tail(call$eval, -h)
  if (h <- call$hide[2,2] %||% 0) call$eval <- head(call$eval, -h)

  call$hide[] <- 0
  call
}
