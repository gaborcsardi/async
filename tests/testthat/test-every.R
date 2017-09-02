
context("every")

test_that("every", {

  skip("need to rewrite with deferred")  
  
  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))

  result <- NULL
  wait_for(every(1:10, is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

  result <- NULL
  wait_for(every(numeric(), is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)

  result <- NULL
  wait_for(every(1:10 * 2 + 1, is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)
})
