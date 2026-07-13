test_that("can have multiple children", {
  do <- async(function() {
    d1 <- delay(1 / 1000)$share()
    d2 <- d1$then(function(.) "foo")
    d3 <- d1$then(function(.) "bar")
    when_all(d2, d3)
  })
  expect_equal(synchronise(do()), list("foo", "bar"))
})

test_that("not cancelled at auto-cancellation", {
  d1 <- d2 <- d3 <- NULL
  do <- async(function() {
    d1 <<- delay(1 / 1000)$share()
    d2 <<- d1$then(function(.) delay(3))
    d3 <<- d2$then(function(.) "foo")
    d4 <- d3$then(function(.) "bar")
    d5 <- d4$catch(error = function(.) "ok")
    d6 <- d1$then(function(.) "bar")$then(function() {
      d4$cancel()
      "ok2"
    })
    when_all(d5, d6)
  })
  expect_equal(synchronise(do()), list("ok", "ok2"))
  expect_false(get_private(d1)$cancelled)
  expect_true(get_private(d3)$cancelled)
})

test_that("shared on an already fulfilled one", {
  do <- function() {
    d1 <- async_constant(42)$share()
    d1$then(function(x) d1$then(function(.) x + 1))
  }
  expect_equal(synchronise(do()), 43)
})
