
context("async_filter")

test_that("async_filter", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  result <- await(async_filter(1:10, is_odd))
  expect_identical(result, c(1L, 3L, 5L, 7L, 9L))

  result <- await(async_filter(numeric(), is_odd))
  expect_identical(result, numeric())

  result <- await(async_filter(1:10 * 2, is_odd))
  expect_identical(result, numeric())
})
