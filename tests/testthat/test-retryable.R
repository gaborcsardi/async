
context("retryable")

test_that("retryable", {

  i <- 1
  f <- function(callback) {
    i <<- i + 1
    if (i < 5) callback("error") else callback(NULL, "OK")
  }

  rf <- retryable(f, 5)

  result <- NULL
  await(rf(callback = function(err, res) result <<- res))

  expect_identical(result, "OK")
})

test_that("retryable, asyncify", {

  i <- 1
  f <- function() {
    i <<- i + 1
    if (i < 5) stop("error") else "OK"
  }

  rf <- retryable(asyncify(f), 5)

  result <- NULL
  await(rf(callback = function(err, res) result <<- res))

  expect_identical(result, "OK")
})
