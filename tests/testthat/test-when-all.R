
context("when_all")

test_that("when_all", {
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) "foo")
    d2 <- delay(1/1000)$then(function(value) "bar")

    dx <- when_all(d1, d2)$
      then(function(value) expect_equal(value, list("foo", "bar")))
    await(dx)
  })
  synchronise(do())
})

test_that("when_all, non-deferred", {
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) "foo")
    d2 <- "bar"

    dx <- when_all(d1, d2)$
      then(function(value) expect_equal(value, list("foo", "bar")))
    await(dx)
  })
  synchronise(do())
})

test_that("when_all, non-deferred only", {
  do <- async(function() {
    d1 <- "foo"
    d2 <- "bar"

    dx <- when_all(d1, d2)$
      then(function(value) expect_equal(value, list("foo", "bar")))
    await(dx)
  })
  synchronise(do())
})

test_that("when_all, empty list", {
  do <- async(function() {
    dx <- when_all()$
      then(function(value) expect_equal(value, list()))
    await(dx)
  })
  synchronise(do())
})

test_that("when_all, error", {
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) stop("foo"))
    d2 <- delay(1/1000)$then(function(value) "bar")

    dx <- when_all(d1, d2)$
      then(NULL, function(reason) expect_match(reason$message, "foo"))
    await(dx)
  })
  synchronise(do())
})

test_that("when_all, multiple errors", {
  do <- async(function() {
    d1 <- delay(1/100  )$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) stop("bar"))

    dx <- when_all(d1, d2)$
      then(NULL, function(reason) expect_match(reason$message, "bar"))
    await(dx)
  })
  synchronise(do())
})
