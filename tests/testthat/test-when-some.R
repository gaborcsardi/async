
context("when_some")

test_that("when_some", {
  d1 <- delay(1/10)$then(function(value) "foo")
  d2 <- delay(1/10000)$then(function(value) "bar")

  dx <- when_some(2, d1, d2)$
    then(function(value) expect_equal(value, list("bar", "foo")))

  wait_for(dx)
})

test_that("when_some, few errors", {
  d1 <- delay(1/10)$then(function(value) "foo")
  d2 <- delay(1/10000)$then(~ stop("ooops"))
  d3 <- delay(1/10000)$then(function(value) "bar")

  dx <- when_some(2, d1, d2, d3)$
    then(function(value) expect_equal(value, list("bar", "foo")))

  wait_for(dx)
})

test_that("too many errors", {
  d1 <- delay(1/10)$then(~ stop("ooops again"))
  d2 <- delay(1/10000)$then(~ stop("ooops"))
  d3 <- delay(1/10000)$then(function(value) "bar")

  dx <- when_some(2, d1, d2, d3)
  expect_error(wait_for(dx), "ooops again")
})
