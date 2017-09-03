
context("some")

test_that("some", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  result <- await(some(1:10, is_odd))
  expect_identical(result, TRUE)

  result <- await(some(numeric(), is_odd))
  expect_identical(result, FALSE)

  result <- await(some(1:10 * 2, is_odd))
  expect_identical(result, FALSE)

})

test_that("some, asyncify", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  result <- await(some(1:10, is_odd))
  expect_identical(result, TRUE)

  result <- await(some(numeric(), is_odd))
  expect_identical(result, FALSE)

  result <- await(some(1:10 * 2, is_odd))
  expect_identical(result, FALSE)
})
