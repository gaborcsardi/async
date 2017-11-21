
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
    frame_id = frame_ids
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

print_wide_stack <- function(stack) {
  for (i in seq_along(stack)) {
    stack[[i]]$same <- paste(
      stack[[i]]$frame_id,
      vcapply(
        lapply(stack[[i]]$calls, format.default),
        paste,
        collapse = "\n"
      )
    )
  }

  df <- do.call(rbind, stack)
  df <- df[ !duplicated(df$frame_id), , drop = FALSE]
  df$out <- vcapply(df$calls, format_call)
  marks <- lapply(stack, function(s) {
    ifelse(df$same %in% s$same, "*", " ")
  })
  df$mark <- do.call(paste, marks)
  cat(paste(df$mark, df$out), sep = "\n")
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
