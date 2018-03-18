
context("async_detect")

test_that("async_detect", {

  is_odd <- async(function(x) {
    force(x)
    delay(1/1000)$
      then(function(value) as.logical(x %% 2))
  })

  test <- async(function(limit) {
    async_detect(1:10, is_odd, .limit = limit)$
      then(~ expect_true(. %in% c(1L, 3L, 5L, 7L, 9L)))

    async_detect(2:10, is_odd, .limit = limit)$
      then(~ expect_true(. %in% c(3L, 5L, 7L, 9L)))

    async_detect(2, is_odd, .limit = limit)$
      then(~ expect_null(.))

    async_detect(c(1:10 * 2L, 43L), is_odd, .limit = limit)$
      then(~ expect_identical(., 43L))

    async_detect(numeric(), is_odd, .limit = limit)$
      then(~ expect_null(.))

    async_detect(1:10 * 2, is_odd, .limit = limit)$
      then(~ expect_null(.))
  })

  lapply(c(Inf, 1, 2, 3, 5, 10, 20), function(x) synchronise(test(x)))
})
