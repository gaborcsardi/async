
test_that("fail, success", {
  did <- 0L
  uns <- function() {
    if (did == 3) {
      "answer"
    } else {
      did <<- did + 1
      message("not yet")
      stop("not yet")
    }
  }

  bo <- function(i) 0.1

  # ok
  did <- 0L
  expect_snapshot(
    synchronise(async_backoff(uns, backoff = bo))
  )

  # not ok
  did <- 0L
  expect_snapshot(
    error = TRUE,
    synchronise(async_backoff(uns, backoff = bo, times = 2))
  )

  # time_limit ok
  did <- 0L
  expect_snapshot(
    synchronise(async_backoff(uns, backoff = bo, time_limit = 1))
  )

  # time_limit not ok
  did <- 0L
  uns2 <- function() {
    suppressMessages(uns())
  }
  expect_snapshot(
    error = TRUE,
    synchronise(async_backoff(uns2, backoff = bo, time_limit = 0.1))
  )
})
