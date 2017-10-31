
context("when_some")

test_that("when_some", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) "foo")
    d2 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_some(2, d1, d2)$
      then(function(value) expect_equal(value, list("bar", "foo")))
  })
  synchronise(do())
})

test_that("when_some, few errors", {
  do <- async(function() {
    d1 <- delay(1/10)$then(function(value) "foo")
    d2 <- delay(1/10000)$then(~ stop("ooops"))
    d3 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_some(2, d1, d2, d3)$
      then(function(value) expect_equal(value, list("bar", "foo")))
  })
  synchronise(do())
})

test_that("too many errors", {
  do <- async(function() {
    d1 <- delay(1/10)$then(~ stop("ooops again"))
    d2 <- delay(1/10000)$then(~ stop("ooops"))
    d3 <- delay(1/10000)$then(function(value) "bar")

    dx <- when_some(2, d1, d2, d3)
    expect_error(await(dx), "ooops again")
  })
  synchronise(do())
})
