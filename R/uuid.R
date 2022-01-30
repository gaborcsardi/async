
get_uuid <- function() {
  async_env$pid <- async_env$pid %||% Sys.getpid()
  async_env$counter <- async_env$counter %||% 0
  async_env$counter <- async_env$counter + 1L
  paste0(async_env$pid, "-", async_env$counter)
}
