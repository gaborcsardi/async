
context("when_all")

test_that("when_all", {
  d1 <- delay(1/1000)$then(function(value) "foo")
  d2 <- delay(1/1000)$then(function(value) "bar")

  dx <- when_all(d1, d2)$
    then(function(value) expect_equal(value, list("foo", "bar")))

  wait_for(dx)
})

test_that("when_all, non-deferred", {
  d1 <- delay(1/1000)$then(function(value) "foo")
  d2 <- "bar"

  dx <- when_all(d1, d2)$
    then(function(value) expect_equal(value, list("foo", "bar")))

  wait_for(dx)
})

test_that("when_all, non-deferred only", {
  d1 <- "foo"
  d2 <- "bar"

  dx <- when_all(d1, d2)$
    then(function(value) expect_equal(value, list("foo", "bar")))

  wait_for(dx)
})

test_that("when_all, empty list", {
  dx <- when_all()$
    then(function(value) expect_equal(value, list()))

  wait_for(dx)
})

test_that("when_all, error", {
  d1 <- delay(1/1000)$then(function(value) stop("foo"))
  d2 <- delay(1/1000)$then(function(value) "bar")

  dx <- when_all(d1, d2)$
    then(NULL, function(reason) expect_equal(reason$message, "foo"))

  wait_for(dx)
})

test_that("when_all, multiple errors", {
  d1 <- delay(1/100  )$then(function(value) stop("foo"))
  d2 <- delay(1/10000)$then(function(value) stop("bar"))

  dx <- when_all(d1, d2)$
    then(NULL, function(reason) expect_equal(reason$message, "bar"))

  wait_for(dx)
})
