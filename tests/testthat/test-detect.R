
context("detect")

test_that("detect", {

  is_odd <- async(function(x) {
    delay(1/1000)$
      then(function(value) as.logical(x %% 2))
  })

  result <- await(detect(1:10, is_odd))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))

  result <- await(detect(2:10, is_odd))
  expect_true(result %in% c(3L, 5L, 7L, 9L))

  result <- await(detect(2, is_odd))
  expect_null(result)

  result <- await(detect(c(1:10 * 2L, 43L), is_odd))
  expect_identical(result, 43L)

  result <- await(detect(numeric(), is_odd))
  expect_null(result)

  result <- await(detect(1:10 * 2, is_odd))
  expect_null(result)
})
