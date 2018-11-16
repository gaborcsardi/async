
#' Async debugging utilities
#'
#' Helper function to help with the non-trivial debugging of async code.
#'
#' Async debugging can be turned on by setting the `async_debug` global
#' option to `TRUE`:
#' ```
#' options(async_debug = TRUE)
#' ```
#' Setting this value to `FALSE` will turn off debugging.
#'
#' If debugging is on, a [synchronise()] call will stop at the beginning
#' of the event loop. No deferred actions of other callbacks have run at
#' this point. [synchronise()] stops by calling [base::browser()]. All the
#' usual [browser()] commands (see its manual) can be used here, plus some
#' extra commands to help async debugging. The extra commands:
#'
#' `async_debug_shortcuts()` adds handy shortcuts to most of the helper
#' functions. E.g. `async_next()` can be invoked as `.an` (without the
#' parens). You only need to run it once per R session. Note that it adds
#' the shortcuts to the global environment.
#'
#' `async_debug_remove_shortcuts()` removes the shortcuts from the global
#' environment.
#'
#' `.an` (or `async_next()`) runs the next iteration of the event loop.
#' Note that it does not return until _something_ happens in the event loop:
#' an action or a parent callback is executed, or HTTP or other I/O is
#' performed. Also note, that a single iteration of the event loop typically
#' runs multiple action, parent or other callbacks. Once the iteration is
#' done, the control is returned to the browser.
#'
#' `.as` (or `async_step()`) is similar to `.an`, but it also starts the
#' debugging of the action or parent callbacks. I.e. another [browser()] is
#' called at the beginning of _all_ callbacks in the next iteration of the
#' event loop.
#'
#' `.asb` (or `async_step_back()`) stops the debugging of the callbacks.
#' It does not actually exdecutes anything from the event loop, so to go
#' back to the main async browser, you also need to execute `c` (continue).
#'
#' `.al` (or `async_list()`) lists all deferred values in the current async
#' phase. (Only the ones that already exist, some may be created in the
#' future.) It returns a data frame with columns:
#'
#' * `id`: The integer id of the deferred value.
#' * `parents`: Integer vector, the parents of the deferred value.
#' * `label`: A character label, that is used by `async_tree()` to nicely
#'    format information about a deferred value.
#' * `call`: The call (language object) that created the deferred value.
#' * `children`: The list of children, an integer vector. A deferred value
#'    can only have one child, unless it is shared.
#' * `type`: The type of the deferred value. This is an arbitrary label,
#'    specified when the deferred value was created.
#' * `running`: Whether the deferred value is already running.
#' * `state`: The state of the deferred value, `"pending"`, `"fulfilled"` or
#'    `"rejected"`. This is typically pending, since resolved deferred
#'    values are removed from the async DAG (in the next event loop
#'    iteration.)
#' * `cancelled`: Whether the deferred value was cancelled.
#' * `shared`: Whether the deferred value is shared.
#' * `filename`: The file name for the source code that created the
#'    deferred value. Only present if this code was parsed with source
#'    references enabled.
#' * `position`: The start file position, in line:column format, as a
#'    string. Only present if this code was parsed with source references
#'    enabled.
#'
#' `.at` (or `async_tree()`) prints the DAG of the deferred values.
#'
#' `async_debug()` can be used to debug the action and/or parent callbacks
#' of the specified deferred value.
#'
#' `async_wait_for()` runs the event loop until the specified deferred
#' value is resolved (i.e. fulfilled or rejected).
#'
#' `.aw` (or `async_where()`) prints a call stack and marks the frame the
#' corresponds to an action or parent callback.
#'
#' @param el Event loop, defaults to the current event loop.
#' @param def Deferred value that is used at the root of the DAG. Defaults
#'   to the deferred value corresponding to the result of the async phase.
#' @param id Integer scalar, the if of the deferred to debug or to wait for.
#' @param action Whether to debug the action callback.
#' @param parent Whether to debug the parent callbacks.
#' @param calls The calls to print, result of `sys.calls()`. Defaults to
#'   the current call stack.
#' @param parents The parent frames in the call stack, result of
#'   `sys.parents()`. Defaults to the current parents.
#' @param frm The async frame to mark. Defaults to the most recent async
#'   frame in the stack.
#'
#' @name async_debug
NULL

#' @export
#' @aliases .an
#' @rdname async_debug

async_next <- function(el = NULL) {
  el <- el %||% find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  if (! el$run("once")) message("[ASYNC] async phase complete")
}

# nocov start

#' @export
#' @aliases .as
#' @rdname async_debug

async_step <- function() {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  old <- options(async_debug_steps = TRUE)
  on.exit(options(old))
  if (! el$run("once")) {
    message("[ASYNC] async phase complete")
  }
}

#' @export
#' @aliases .asb
#' @rdname async_debug

async_step_back <- function() {
  options(async_debug_steps = FALSE)
  message("[ASYNC] step back, you still need to 'c'ontinue")
}

# nocov end

#' @export
#' @aliases .al
#' @rdname async_debug

async_list <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  info <- list()
  find_parents <- function(def) {
    info <<- c(info, list(get_private(def)$get_info()))
    prn <- get_private(def)$parents
    lapply(prn, find_parents)
  }
  find_parents(def)

  do.call(rbind, info)
}

#' @export
#' @aliases .at
#' @rdname async_debug

async_tree <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  data <- async_list(def)
  root <- as.character(def$get_id())
  cli::tree(data, root = root)
}

#' @export
#' @rdname async_debug

async_debug <- function(id, action = TRUE, parent = TRUE) {
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  prv <- get_private(def)

  if (prv$state != "pending") {
    message("[ASYNC] ", id, " already resolved")
    return(invisible())
  }

  what <- character()
  if (action) {
    if (prv$running) {
      message("[ASYNC] ", id, " action already running")
    } else if (is.null(prv$action)) {
      message("[ASYNC] ", id, " has no action")
    } else {
      ## TODO: make a copy? Or should the deferred make a copy?
      debug1(prv$action)
      what <- "action"
    }
  }

  if (parent) {
    ## TODO: make copies?
    debug_all(prv$parent_resolve)
    debug_all(prv$parent_reject)
    what <- c(what, "parent callbacks")
  }

  if (length(what) == 1) {
    message("[ASYNC] ", id, " debugging ", what)
  }
  if (length(what) == 2) {
    message("[ASYNC] ", id, " debugging ", what[1], " and ", what[2])
  }

  invisible(def)
}

#' @export
#' @rdname async_debug

async_wait_for <- function(id) {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  priv <- get_private(def)
  while (priv$state == "pending") el$run("once")
  message("[ASYNC] ", id, "  resolved")
}

#' @export
#' @aliases .aw
#' @rdname async_debug

async_where <- function(calls = sys.calls(), parents = sys.parents(),
                        frm = get_async_frames()) {
  afrm <- viapply(frm, "[[", "frame")
  num <- seq_along(calls)

  src <- lapply(calls, get_source_position)

  res <- data.frame(
    stringsAsFactors = FALSE,
    call = I(calls),
    parent = parents,
    filename = vcapply(src, "[[", "filename"),
    position = vcapply(src, "[[", "position"),
    async = num %in% afrm
  )

  res$def_id <- NA_integer_
  res$def_id[afrm] <- viapply(frm, function(x) x$deferred)
  res$def_cb_type <- NA_character_
  res$def_cb_type[afrm] <- vcapply(frm, function(x) x$type)
  res$def_call <- I(list(NULL))
  res$def_call[afrm] <- lapply(frm, "[[", "call")

  def_src <- lapply(res$def_call[afrm], get_source_position)
  res$def_filename <- NA_character_
  res$def_filename[afrm] <- vcapply(def_src, "[[", "filename")
  res$def_position <- NA_character_
  res$def_position[afrm] <- vcapply(def_src, "[[", "position")

  class(res) <- c("async_where", class(res))
  res
}

# nocov start

#' @export

print.async_where <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

#' @export

format.async_where <- function(x, ...) {
  paste0(paste(
    formatC(seq_len(nrow(x)), width = 3),
    vcapply(x$call, expr_name),
    paste0(" ", x$filename, ":", x$position),
    ifelse (! x$async, "",
            paste0("\n    ", x$def_id, " ", x$def_cb_type, " ",
                   x$def_call, " ", x$def_filename, ":", x$def_position)),
    collapse = "\n"
  ), "\n")
}

get_async_frames <- function() {
  drop_nulls(lapply(seq_along(sys.frames()), function(i) {
    if (! is.null(data <- sys.frame(i)$`__async_data__`)) {
      list(frame = i + data$skip %||% 1L, deferred = data[[1]], type = data[[2]],
           call = get_private(data[[3]])$mycall)
    }
  }))
}

find_sync_frame <- function() {
  for (i in seq_along(sys.frames())) {
    cand  <- sys.frame(-i)
    if (isTRUE(cand$`__async_synchronise_frame__`)) return(cand)
  }
}

find_async_data_frame <- function() {
  frames <- sys.frames()
  for (i in seq_along(frames)) {
    cand  <- sys.frame(-i)
    if (!is.null(data <- cand$`__async_data__`)) {
      return(list(frame = length(frames) - i + 1L, data = data))
    }
  }
}

find_deferred <- function(id, def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  search_parents <- function(def) {
    if (def$get_id() == id) return(def)
    prn <- get_private(def)$parents
    for (p in lapply(prn, search_parents)) {
      if (!is.null(p)) return(p)
    }
  }
  search_parents(def)
}

# nocov start

debug1 <- function(fun) {
  debugonce(fun)
}

#' @export
#' @rdname async_debug

async_debug_shortcuts <- function() {
  as <- function(name, fun) {
    makeActiveBinding(name, fun, .GlobalEnv)
  }
  as(".an", async_next)
  as(".as", async_step)
  as(".asb", async_step_back)
  as(".al", async_list)
  as(".at", async_tree)
  as(".aw", async_where)
}

#' @export
#' @rdname async_debug

async_debug_remove_shortcuts <- function() {
  tryCatch(
    rm(list = c(".an", ".as", ".asb", ".al", ".at", ".aw"),
       envir = .GlobalEnv),
    error = function(x) x)
}

# nocov end

debug_all <- function(fun) {
  debug(fun)
}
