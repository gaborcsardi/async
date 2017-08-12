
context("every")

test_that("every", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))

  result <- NULL
  every(1:10, is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, FALSE)

  result <- NULL
  every(numeric(), is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, TRUE)

  result <- NULL
  every(1:10 * 2 + 1, is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, TRUE)
})
