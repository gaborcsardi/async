
context("some")

test_that("some", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))

  result <- NULL
  some(1:10, is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, TRUE)

  result <- NULL
  some(numeric(), is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, FALSE)

  result <- NULL
  some(1:10 * 2, is_odd, function(err, res) result <<- res)
  await_all()
  expect_identical(result, FALSE)

})
