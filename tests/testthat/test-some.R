
context("async_some")

test_that("async_some", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  result <- sync_wrap(async_some(1:10, is_odd))
  expect_identical(result, TRUE)

  result <- sync_wrap(async_some(numeric(), is_odd))
  expect_identical(result, FALSE)

  result <- sync_wrap(async_some(1:10 * 2, is_odd))
  expect_identical(result, FALSE)

})

test_that("async_some, asyncify", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  result <- sync_wrap(async_some(1:10, is_odd))
  expect_identical(result, TRUE)

  result <- sync_wrap(async_some(numeric(), is_odd))
  expect_identical(result, FALSE)

  result <- sync_wrap(async_some(1:10 * 2, is_odd))
  expect_identical(result, FALSE)
})
