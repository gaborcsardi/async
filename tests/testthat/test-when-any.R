
context("when_any")

test_that("when_any", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) "foo")
    d2 <- delay(1/10000)$then(function(value) "bar")

    when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))
  })
  synchronise(do())
})

test_that("when_any, non-deferred", {
  do <- async(function() {
    d1 <- delay(1/100)$then(function(value) "foo")
    d2 <- "bar"

    when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))$
      then(~ d1)$
      catch(error = identity)
  })
  synchronise(do())
})

test_that("when_any, non-deferred only", {
  do <- async(function() {
    d1 <- "foo"
    d2 <- "bar"

    dx <- when_any(d1, d2)$
      then(function(value) expect_true(value %in% c("foo", "bar")))
  })
  synchronise(do())
})

test_that("when_any, error first, success then", {
  do <- async(function() {
    d1 <- delay(1/10000)$then(function(value) stop("foo"))
    d2 <- delay(1/10)$then(function(value) "bar")

    dx <- when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))
  })
  synchronise(do())
})

test_that("when_any, late error is ignored", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_any(d1, d2)$
      catch(error = function(value) expect_equal(value, "bar"))
  })
  expect_silent(synchronise(do()))
})

test_that("when_any, multiple errors", {
  do <- async(function() {
    d1 <- delay(1/100  )$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) stop("bar"))

    dx <- when_any(d1, d2)$
      catch(error = function(reason) {
        expect_match(conditionMessage(reason$errors[[1]]), "bar")
        expect_match(conditionMessage(reason$errors[[2]]), "foo")
      })
  })
  synchronise(do())
})
