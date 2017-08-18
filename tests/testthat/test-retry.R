
context("retry")

test_that("unsuccessful retry", {
  x <- 5
  err <- res <- NULL
  await(retry(
    function(callback) {
      x <<- x - 1
      if (x) callback("error") else callback(NULL, "OK")
    },
    times = 3,
    function(err, res) {
      err <<- err
      res <<- res
    }
  ))

  expect_equal(err, "error")
  expect_null(res)
})

test_that("successful retry", {
  x <- 5
  err <- res <- NULL
  await(retry(
    function(callback) {
      x <<- x - 1
      if (x) callback("error") else callback(NULL, "OK")
    },
    times = 5,
    function(err, res) {
      err <<- err
      res <<- res
    }
  ))

  expect_null(err)
  expect_equal(res, "OK")
})

test_that("retry and asyncify", {

  fun <- function() {
    x <<- x - 1
    if (x) stop("Error") else "OK"
  }

  x <- 5
  err <- res <- NULL
  await(retry(
    asyncify(fun),
    times = 5,
    function(err, res) {
      err <<- err
      res <<- res
    }
  ))

  expect_null(err)
  expect_equal(res, "OK")
})
