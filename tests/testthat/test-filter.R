
context("async_filter")

test_that("async_filter", {

  is_odd <- function(x) {
    force(x)
    delay(1/1000)$then(function(value) as.logical(x %% 2))
  }

  do <- function() {
    d1 <- async_filter(1:10, is_odd)$
      then(~ expect_identical(., c(1L, 3L, 5L, 7L, 9L)))

    d2 <- async_filter(numeric(), is_odd)$
      then(~ expect_identical(., numeric()))

    d3 <- async_filter(1:10 * 2, is_odd)$
      then(~ expect_identical(., numeric()))

    when_all(d1, d2, d3)
  }
  synchronise(do())
})
