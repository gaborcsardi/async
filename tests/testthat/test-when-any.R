
context("when_any")

test_that("when_any", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) "foo")
    d2 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))
  })
  sync_wrap(do())
})

test_that("when_any, non-deferred", {
  do <- async(function() {
    d1 <- delay(1/100)$then(function(value) "foo")
    d2 <- "bar"

    dx <- when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))
  })
  sync_wrap(do())
})

test_that("when_any, non-deferred only", {
  do <- async(function() {
    d1 <- "foo"
    d2 <- "bar"

    dx <- when_any(d1, d2)$
      then(function(value) expect_true(value %in% c("foo", "bar")))
  })
  sync_wrap(do())
})

test_that("when_any, error first, success then", {
  do <- async(function() {
    d1 <- delay(1/10000)$then(function(value) stop("foo"))
    d2 <- delay(1/10)$then(function(value) "bar")

    dx <- when_any(d1, d2)$
      then(function(value) expect_equal(value, "bar"))
  })
  sync_wrap(do())
})

test_that("when_any, late error is ignored", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_any(d1, d2)$
      then(NULL, function(value) expect_equal(value, "bar"))
  })
  expect_silent(sync_wrap(do()))
})

test_that("when_any, multiple errors", {
  do <- async(function() {
    d1 <- delay(1/100  )$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) stop("bar"))

    dx <- when_any(d1, d2)$
      then(NULL, function(reason) expect_match(reason$message, "foo"))
  })
  sync_wrap(do())
})
