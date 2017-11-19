
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
  barriers <- find_calls_in_stack(calls, quote(async_start_task))
  defs <- lapply(frames[barriers], get_deferred_from_barrier)
  async_ids <- vcapply(defs, function(x) env_name(x$self))

  current_stack <- data.frame(
    stringsAsFactors = FALSE,
    task_id = tail(c("main", async_ids), 1),
    calls = I(calls),
    frame_id = frame_ids
  )

  parent_stacks <- lapply(defs, function(x) x$private$start_stack)
  stacks <- c(unlist(parent_stacks, recursive = FALSE), list(current_stack))
  task_names <- vcapply(stacks, function(x) x$task_id[[1]])
  stacks[! duplicated(task_names)]
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
