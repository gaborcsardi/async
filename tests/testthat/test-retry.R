
context("retry")

test_that("unsuccessful retry", {
  x <- 5
  err <- res <- NULL
  await(retry(
    function(callback) {
      x <<- x - 1
      if (x) callback("error") else callback(NULL, "OK")
    },
    function(err, res) {
      err <<- err
      res <<- res
    },
    3
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
    function(err, res) {
      err <<- err
      res <<- res
    },
    5
  ))

  expect_null(err)
  expect_equal(res, "OK")
})
