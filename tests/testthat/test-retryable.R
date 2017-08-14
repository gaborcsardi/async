
context("retryable")

test_that("retryable", {

  i <- 1
  f <- function(callback) {
    i <<- i + 1
    if (i < 5) callback("error") else callback(NULL, "OK")
  }

  rf <- retryable(f)

  result <- NULL
  await(rf(callback = function(err, res) result <<- res, times = 5))

  expect_identical(result, "OK")
})
