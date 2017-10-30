
context("when_any")

test_that("when_any", {
  d1 <- delay(1/100)$then(function(value) "foo")
  d2 <- delay(1/10000)$then(function(value) "bar")

  dx <- when_any(d1, d2)$
    then(function(value) expect_equal(value, "bar"))

  await(dx)
})

test_that("when_any, non-deferred", {
  d1 <- delay(1/100)$then(function(value) "foo")
  d2 <- "bar"

  dx <- when_any(d1, d2)$
    then(function(value) expect_equal(value, "bar"))

  await(dx)
})

test_that("when_any, non-deferred only", {
  d1 <- "foo"
  d2 <- "bar"

  dx <- when_any(d1, d2)$
    then(function(value) expect_true(value %in% c("foo", "bar")))

  await(dx)
})

test_that("when_any, error first, success then", {
  d1 <- delay(1/10000)$then(function(value) stop("foo"))
  d2 <- delay(1/100)$then(function(value) "bar")

  dx <- when_any(d1, d2)$
    then(function(value) expect_equal(value, "bar"))

  await(dx)
})

test_that("when_any, late error is ignored", {
  d1 <- delay(1/100)$then(function(value) stop("foo"))
  d2 <- delay(1/10000)$then(function(value) "bar")

  dx <- when_any(d1, d2)$
    then(NULL, function(value) expect_equal(value, "bar"))

  await(dx)
})

test_that("when_any, multiple errors", {
  d1 <- delay(1/100  )$then(function(value) stop("foo"))
  d2 <- delay(1/10000)$then(function(value) stop("bar"))

  dx <- when_any(d1, d2)$
    then(NULL, function(reason) expect_match(reason$message, "foo"))

  await(dx)
})
