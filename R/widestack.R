
env_name <- function(env) {
  if ((n <- environmentName(env)) != "") {
    n
  } else {
    sub(">$", "", sub("^<environment: ", "", format.default(env)))
  }
}

#' @importFrom utils head

record_stack <- function() {
  calls <- head(sys.calls(), -1)
  frames <- head(sys.frames(), -1)
  record_this_stack(calls, frames)
}

#' @importFrom utils tail

record_this_stack <- function(calls, frames) {
  frame_ids <- vcapply(frames, env_name)

  ## The async tasks currently initializing
  init_barriers <- find_calls_in_stack(calls, quote(async_def_init))
  init_defs <- lapply(frames[init_barriers], get_deferred_from_barrier)

  ## The async tasks currently running action or callback
  run_barriers  <- find_calls_in_stack(calls, quote(async_stack_run))
  run_defs  <- lapply(frames[run_barriers],  get_deferred_from_barrier)

  barriers <- init_barriers | run_barriers
  defs <- lapply(frames[barriers],  get_deferred_from_barrier)
  async_ids <- vcapply(defs, function(x) env_name(x$self))

  ## What to hide? From a hide barrier to the next barrier
  hide_barriers <- which(find_calls_in_stack(calls, quote(async_hide)))
  barrier_idx <- which(barriers)
  next_barriers <- viapply(
    hide_barriers,
    function(b) min(barrier_idx[barrier_idx > b] - 1L, length(calls))
  )
  to_hide <- logical(length(calls))
  for (i in seq_along(hide_barriers)) {
    to_hide[hide_barriers[i]:next_barriers[i]] <- TRUE
  }

  ## Also hide two calls before async_def_init
  to_hide[which(init_barriers) - 1] <- TRUE
  to_hide[which(init_barriers) - 2] <- TRUE

  ## Active async task, if any, either initializing or running
  last_async_frame <- tail(frames[barriers], 1)
  act_task <- if (length(last_async_frame)) last_async_frame[[1]]$deferred
  act_parent <- act_task$.__enclos_env__$private$parent
  act_id <- if (length(act_task)) env_name(act_task) else "main"
  act_parent_id <- if (length(act_parent)) env_name(act_parent) else NA_character_

  call_df <- data.frame(
    stringsAsFactors = FALSE,
    task_id = c("main", async_ids)[cumsum(barriers) + 1],
    call = I(calls),
    frame_id = frame_ids,
    hide = to_hide,
    init = init_barriers,
    callback = run_barriers
  )

  ## Wide stacks of the prelude tasks, we need to make them unique. (TODO)
  ## Note that these are the start stacks only. Run stacks of a deferred
  ## might be included in another deferred's stack.
  pre <- c(defs, list(list(private = act_parent$.__enclos_env__$private)))
  pre_stacks <- drop_nulls(lapply(pre, function(x) x$private$start_stack))

  ## Merge the prelude to the current wide stack.
  ## `calls` is a named list of call stack data frames
  ## `parents` is a character vector
  extract_calls <- function(x) {
    unlist(lapply(x, "[[", "calls"), recursive = FALSE)
  }

  wsts <- list(
    calls = c(
      unique_names(extract_calls(pre_stacks)),
      structure(list(call_df), names = act_id)
    ),
    parents = unique_names(c(
      unlist(lapply(pre_stacks, "[[", "parents")),
      structure(act_parent_id, names = act_id)
    ))
  )

  wsts
}

get_deferred_from_barrier <- function(frame) {
  list(
    self = frame$deferred,
    private = frame$deferred$.__enclos_env__$private
  )
}

print_wide_stack <- function(wst) {

  add_hash <- function(x) {
    x$same <- paste(
      x$frame_id,
      vcapply(lapply(x$call, format.default), paste, collapse = "\n")
    )
    x
  }
  stack <- lapply(wst$calls, add_hash)

  df <- do.call(rbind, stack)
  df <- df[ !duplicated(df$same), , drop = FALSE]

  df$out <- vcapply(df$call, format_call)

  ## Marks
  marks <- lapply(unique(df$task_id), function(t) {
    ifelse(df$task_id == t, "*", " ")
  })
  df$marks <- do.call(
    mapply,
    c(list(c), marks, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  )

  pdf <- df[! df$hide, ]

  ## Lift init calls up one step to avoid duplication
  init_calls <- which(pdf$init) - 1L
  if (length(init_calls)) {
    pdf$out[init_calls] <- paste("+", pdf$out[init_calls])
  }

  compose <- function(x, y) ifelse(x == " " & y == " ", " ", "*")

  pdf$marks[init_calls] <- mapply(
    compose,
    pdf$marks[init_calls], pdf$marks[init_calls + 1], SIMPLIFY=FALSE
  )
  pdf <- pdf[! pdf$init, ]

  ## rewrite async_stack_run() calls to something more descriptive
  run_calls <- which(pdf$callback)
  if (length(run_calls)) {
    pdf$out[run_calls] <-
      paste(">", sub("^[+][ ]", "", pdf$out[run_calls - 1L]))
  }

  ## TODO: remove extra func() and func(value) calls from callback

  add_arrows <- function(x) {
    w <- regexpr("[*][ ]+[*]", x)
    regmatches(x, w) <- gsub(" ", "-", regmatches(x, w))
    x
  }

  pdf$mark <- vcapply(pdf$marks, paste, collapse = " ")
  pdf$mark <- add_arrows(pdf$mark)

  cat(paste(pdf$mark, pdf$out), sep = "\n")
}

format_call <- function(call) {
  out <- format.default(call)
  if (length(out) > 1) out <- paste0(out[1], "...")
  out
}

#' @export

print.async_rejected <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export

format.async_rejected <- function(x, ...) {
  ## TODO
  format.default(x)
}

#' @export

conditionCall.async_rejected <- function(c) {
  ## TODO
  c$call
}

#' @export

conditionMessage.async_rejected <- function(c) {
  ## TODO
  c$message
}
