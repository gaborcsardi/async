
context("retryable")

test_that("retryable", {

  i <- 1
  f <- function() {
    i <<- i + 1
    if (i < 5) stop("error") else "OK"
  }

  rf <- retryable(f, 5)
  result <- await(rf())
  expect_identical(result, "OK")
})
