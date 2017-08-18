
context("filter")

test_that("filter", {

  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))
  save <- function(err, res) result <<- res

  result <- NULL
  wait_for(filter(1:10, is_odd, save))
  expect_identical(result, c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  wait_for(filter(numeric(), is_odd, save))
  expect_identical(result, numeric())

  result <- NULL
  wait_for(filter(1:10 * 2, is_odd, save))
  expect_identical(result, numeric())
})

test_that("filter, asyncify", {

  is_odd <- asyncify(function(x) as.logical(x %% 2))
  save <- function(err, res) result <<- res

  result <- NULL
  wait_for(filter(1:10, is_odd, save))
  expect_identical(result, c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  wait_for(filter(numeric(), is_odd, save))
  expect_identical(result, numeric())

  result <- NULL
  wait_for(filter(1:10 * 2, is_odd, save))
  expect_identical(result, numeric())
})
