
async_longstack_barrier <- function(fun) {
  attr(fun, "async")$barrier <- TRUE
  fun
}
