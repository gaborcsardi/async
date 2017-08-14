
context("every")

test_that("every", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))

  result <- NULL
  await(every(1:10, is_odd, function(err, res) result <<- res))
  expect_identical(result, FALSE)

  result <- NULL
  await(every(numeric(), is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)

  result <- NULL
  await(every(1:10 * 2 + 1, is_odd, function(err, res) result <<- res))
  expect_identical(result, TRUE)
})
