
test_that("async_sequence", {

  add1 <- function(n) { n ; delay(10/1000)$then(function(value) n + 1) }
  mul3 <- function(n) { n ; delay(10/1000)$then(function(value) n * 3) }

  add1mul3 <- async_sequence(add1, mul3)
  result <- synchronise(add1mul3(4))

  expect_equal(result, 15)
})
