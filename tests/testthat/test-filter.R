
context("filter")

test_that("filter", {

  is_odd <- function(x, cb) cb(NULL, as.logical(x %% 2))
  save <- function(err, res) result <<- res

  result <- NULL
  filter(1:10, is_odd, save)
  await_all()
  expect_identical(result, c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  filter(numeric(), is_odd, save)
  await_all()
  expect_identical(result, numeric())

  result <- NULL
  filter(1:10 * 2, is_odd, save)
  await_all()
  expect_identical(result, numeric())
})
