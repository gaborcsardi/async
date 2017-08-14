
context("some")

test_that("some", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))

  result <- NULL
  await(some(1:10, is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)

  result <- NULL
  await(some(numeric(), is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

  result <- NULL
  await(some(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

})
