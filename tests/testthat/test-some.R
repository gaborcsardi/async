
context("some")

test_that("some", {

  skip("need to rewrite with deferred")  
  
  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))

  result <- NULL
  wait_for(some(1:10, is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)

  result <- NULL
  wait_for(some(numeric(), is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

  result <- NULL
  wait_for(some(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

})

test_that("some, asyncify", {

  skip("need to rewrite with deferred")  
  
  is_odd <- asyncify(function(x) as.logical(x %% 2))

  result <- NULL
  wait_for(some(1:10, is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)

  result <- NULL
  wait_for(some(numeric(), is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

  result <- NULL
  wait_for(some(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)
})
