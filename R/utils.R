
`%||%` <- function(l, r) if (is.null(l)) r else l

vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

viapply <- function(X, FUN, ..., FUN.VALUE = integer(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

make_error <- function(message, class = "simpleError", call = NULL) {
  class <- c(class, "error", "condition")
  structure(
    list(message = as.character(message), call = call),
    class = class
  )
}

num_args <- function(fun) {
  length(formals(fun))
}

get_private <- function(x) {
  x$.__enclos_env__$private
}

#' Call `func` and then call `callback` with the result
#'
#' `callback` will be called with two arguments, the first one will the
#' error object if `func()` threw an error, or `NULL` otherwise. The second
#' argument is `NULL` on error, and the result of `func()` otherwise.
#'
#' @param func Function to call.
#' @param callback Callback to call with the result of `func()`,
#'   or the error thrown.
#' @param info Extra info to add to the error object. Must be a named list.
#'
#' @keywords internal

call_with_callback <- function(func, callback, info = NULL) {
  recerror <- NULL
  result <- NULL
  tryCatch(
    withCallingHandlers(
      result <- func(),
      error = function(e) {
        recerror <<- e
        recerror$aframe <<- recerror$aframe %||% find_async_data_frame()
        recerror$calls <<- recerror$calls %||% sys.calls()
        if (is.null(recerror[["call"]])) recerror[["call"]] <<- sys.call()
        recerror$parents <<- recerror$parents %||% sys.parents()
        recerror[names(info)] <<- info
        handler <- getOption("async.error")
        if (is.function(handler)) handler()
      }
    ),
    error = identity
  )
  callback(recerror, result)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

new_event_loop_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

lapply_args <- function(X, FUN, ..., .args = list()) {
  do.call("lapply", c(list(X = X, FUN = FUN), list(...), .args))
}

drop_nulls <- function(x) {
  x[!vlapply(x, is.null)]
}

#' @importFrom utils getSrcDirectory getSrcFilename getSrcLocation

get_source_position <- function(call) {
  list(
    filename = file.path(
      c(getSrcDirectory(call), "?")[1],
      c(getSrcFilename(call), "?")[1]),
    position = paste0(
      getSrcLocation(call, "line", TRUE) %||% "?", ":",
      getSrcLocation(call, "column", TRUE) %||% "?")
  )
}

file_size <- function(...) {
  file.info(..., extra_cols = FALSE)$size
}

read_all <- function(filename, encoding) {
  if (is.null(filename)) return(NULL)
  r <- readBin(filename, what = raw(0), n = file_size(filename))
  s <- rawToChar(r)
  Encoding(s) <- encoding
  s
}

px_file_type <- function(file) {
  if (!is_string(file)) {
    return("NULL")
  }

  switch(
    basename(file),
    "2>&1" = "NULL",
    "|" = "conn",
    "file"
  )
}

px_conns <- function(px) {
  compact(list(
    process = if (px$has_poll_connection()) px$get_poll_connection(),
    stdout = if (px$has_output_connection()) px$get_output_connection(),
    stderr = if (px$has_error_connection()) px$get_error_connection()
  ))
}

px_buffers <- function(px) {
  compact(list(
    stdout = if (px$has_output_connection()) make_buffer(),
    stderr = if (px$has_error_connection()) make_buffer()
  ))
}

make_buffer <- function() {
  con <- file(open = "w+b")

  size <- 0L
  list(
    push = function(text) {
      size <<- size + nchar(text, type = "bytes")
      cat(text, file = con)
    },
    read = function() {
      readChar(con, size, useBytes = TRUE)
    },
    done = function() {
      close(con)
    }
  )
}

crash <- function () {
  get("attach")(structure(list(), class = "UserDefinedDatabase"))
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

expr_name <- function(expr) {
  if (is.null(expr)) {
    return("NULL")
  }

  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  if (is.call(expr)) {
    cl <- as.list(expr)[[1]]
    if (is.symbol(cl)) {
      return(as.character(cl))
    } else {
      return(paste0(format(cl), collapse = ""))
    }
  }

  if (is.atomic(expr) && length(expr) == 1) {
    return(as.character(expr))
  }

  gsub("\n.*$", "...", as.character(expr))
}

data.frame <- function(..., stringsAsFactors = FALSE) {
  base::data.frame(..., stringsAsFactors = stringsAsFactors)
}
