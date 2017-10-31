
context("async_detect")

test_that("async_detect", {

  is_odd <- async(function(x) {
    force(x)
    delay(1/1000)$
      then(function(value) as.logical(x %% 2))
  })

  test <- async(function(limit) {
    result <- await(async_detect(1:10, is_odd, .limit = limit))
    expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))

    result <- await(async_detect(2:10, is_odd, .limit = limit))
    expect_true(result %in% c(3L, 5L, 7L, 9L))

    result <- await(async_detect(2, is_odd, .limit = limit))
    expect_null(result)

    result <- await(async_detect(c(1:10 * 2L, 43L), is_odd, .limit = limit))
    expect_identical(result, 43L)

    result <- await(async_detect(numeric(), is_odd, .limit = limit))
    expect_null(result)

    result <- await(async_detect(1:10 * 2, is_odd, .limit = limit))
    expect_null(result)
  })

  lapply(c(Inf, 1, 2, 3, 5, 10, 20), function(x) synchronise(test(x)))
})
