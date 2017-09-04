
context("async_retryable")

test_that("async_retryable", {

  i <- 1
  f <- function() {
    i <<- i + 1
    if (i < 5) stop("error") else "OK"
  }

  rf <- async_retryable(f, 5)
  result <- await(rf())
  expect_identical(result, "OK")
})
