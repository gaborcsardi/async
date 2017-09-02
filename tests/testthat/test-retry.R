
context("retry")

test_that("unsuccessful retry", {

  skip("need to rewrite with deferred")  
  
  x <- 5
  err <- res <- NULL
  wait_for(retry(
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

  skip("need to rewrite with deferred")  
  
  x <- 5
  err <- res <- NULL
  wait_for(retry(
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

  skip("need to rewrite with deferred")  
  
  fun <- function() {
    x <<- x - 1
    if (x) stop("Error") else "OK"
  }

  x <- 5
  err <- res <- NULL
  wait_for(retry(
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
