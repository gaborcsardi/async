
context("detect")

test_that("detect", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))

  result <- NULL
  await(detect(1:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  await(detect(2:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(3L, 5L, 7L, 9L))

  result <- "blah"
  await(detect(2, is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- NULL
  await(detect(c(1:10 * 2L, 43L), is_odd,
               function(err, res) result <<- res))
  expect_identical(result, 43L)

  result <- "blah"
  await(detect(numeric(), is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- "blah"
  await(detect(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_null(result)
})
