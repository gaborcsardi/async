
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
  act_parent <- deferred$.__enclos_env__$private$parent
  act_id <- if (length(act_task)) env_name(act_task) else "main"

  wst <- data.frame(
    stringsAsFactors = FALSE,
    task_id = c("main", async_ids)[cumsum(barriers) + 1],
    call = I(calls),
    frame_id = frame_ids
  )

  ## up_stacks <- lapply(defs, function(x) x$private$start_stack)
  ## parent_stacks <- lapply(
  ##   defs,
  ##   function(x) x$private$parent$.__enclos_env__$private$start_stack
  ## )

  ## stacks <- c(
  ##   unlist(parent_stacks, recursive = FALSE),
  ##   unlist(up_stacks, recursive = FALSE),
  ##   list(current_stack))
  ## task_names <- vcapply(stacks, function(x) x$task_id[[1]])
  ## stacks[task_names == "main" | !duplicated(task_names)]


  browser()

  ## current_stack <- data.frame(
  ##   stringsAsFactors = FALSE,
  ##   task_id = tail(c("main", async_ids), 1),
  ##   calls = I(calls),
  ##   frame_id = frame_ids
  ## )

  ## up_stacks <- lapply(defs, function(x) x$private$start_stack)
  ## parent_stacks <- lapply(
  ##   defs,
  ##   function(x) x$private$parent$.__enclos_env__$private$start_stack
  ## )

  ## stacks <- c(
  ##   unlist(parent_stacks, recursive = FALSE),
  ##   unlist(up_stacks, recursive = FALSE),
  ##   list(current_stack))
  ## task_names <- vcapply(stacks, function(x) x$task_id[[1]])
  ## stacks[task_names == "main" | !duplicated(task_names)]
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
