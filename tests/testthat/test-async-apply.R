
context("async_apply")

test_that("async_apply", {
  f <- function(a, b, c, callback) callback(NULL, sum(a, b, c))
  af <- async_apply(f, a = 1, b = 2, c = 3)

  result <- NULL
  wait_for(parallel(
    list(af),
    function(err, res) result <<- res
  ))
  expect_equal(result, list(6))
})

test_that("no arguments", {
  f <- function(callback) callback(NULL, 42)
  af <- async_apply(f)

  result <- NULL
  wait_for(parallel(
    list(af),
    function(err, res) result <<- res
  ))
  expect_equal(result, list(42))
})
