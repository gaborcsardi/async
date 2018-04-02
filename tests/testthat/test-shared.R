
context("shared")

test_that("can have multiple children", {
  do <- async(function() {
    d1 <- delay(1/1000)$share()
    d2 <- d1$then(~ "foo")
    d3 <- d1$then(~ "bar")
    when_all(d2, d3)
  })
  expect_equal(synchronise(do()), list("foo", "bar"))
})

test_that("not cancelled at auto-cancellation", {
  d1 <- d2 <- NULL
  do <- async(function() {
    d1 <<- delay(1/1000)$share()
    d2 <<- d1$then(~ delay(3))
    d3 <- d2$then(~ "foo")
    d4 <- d3$catch(error = ~ "ok")
    d5 <- d1$then(~ "bar")$then(function() { d3$cancel(); "ok2" })
    when_all(d4, d5)
  })
  expect_equal(synchronise(do()), list("ok", "ok2"))
  expect_false(get_private(d1)$cancelled)
  expect_true(get_private(d2)$cancelled)
})

test_that("shared on an already fulfilled one", {

  do <- function() {
    d1 <- async_constant(42)$share()
    d1$then(function(x) d1$then(~ x + 1))
  }
  expect_equal(synchronise(do()), 43)
})
