
context("some")

test_that("some", {

  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))

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

test_that("some, asyncify", {

  is_odd <- asyncify(function(x) as.logical(x %% 2))

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
