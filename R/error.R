
#' @export

print.async_deferred_rejected <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.async_deferred_rejected <- function(x, ...) {
  c(paste0("Async error: ", x$message), "", format_long_stack(x$call))
}

format_long_stack <- function(call) {

  separator <- function() "  --------\n"
  result <- character()

  if (!is.null(call$parent)) {
    result <- c(result, format_long_stack(call$parent), separator())
  }

  call <- trim_long_stack(call)

  result <- c(
    result,
    paste(" ", seq_along(call$start), as.character(call$start))
  )
  if (length(call$eval)) {
    result <- c(
      result,
      separator(),
      paste(" ", seq_along(call$eval), as.character(call$eval))
    )
  }

  result
}

#' @importFrom utils head tail

trim_long_stack <- function(call) {
  if (h <- call$hide[1,1] %||% 0) call$start <- tail(call$start, -h)
  if (h <- call$hide[1,2] %||% 0) call$start <- head(call$start, -h)
  if (h <- call$hide[2,1] %||% 0 && is.list(call$eval)) {
    call$eval <- tail(call$eval, -h)
  }
  if (h <- call$hide[2,2] %||% 0 && is.list(call$eval)) {
    call$eval <- head(call$eval, -h)
  }

  call$hide[] <- 0
  call
}

#' @export

conditionCall.async_deferred_rejected <- function(c) {
  call <- trim_long_stack(c$call)
  st <- if (length(call$eval)) call$eval else call$start
  if (is.list(st)) tail(st, 1)[[1]] else st
}

#' @export

conditionMessage.async_deferred_rejected <- function(c) {
  paste(c("", format(c)), collapse = "\n")
}
