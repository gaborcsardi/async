
context("async_filter")

test_that("async_filter", {

  is_odd <- function(x) {
    force(x)
    delay(1/1000)$then(function(value) as.logical(x %% 2))
  }

  do <- function() {
    d1 <- async_filter(1:10, is_odd)$
      then(function(.) expect_identical(., c(1L, 3L, 5L, 7L, 9L)))

    d2 <- async_filter(numeric(), is_odd)$
      then(function(.) expect_identical(., numeric()))

    d3 <- async_filter(1:10 * 2, is_odd)$
      then(function(.) expect_identical(., numeric()))

    when_all(d1, d2, d3)
  }
  synchronise(do())
})

test_that("async_filter, errors", {

  called <- FALSE
  do <- function()  {
    async_filter(1:10, function(x) stop("doh"))$
      then(function() called <<- TRUE)$
      catch(error = function(e) {
        expect_equal(conditionMessage(e), "doh")
        expect_s3_class(e, "async_rejected")
      })
  }

  synchronise(do())
  expect_false(called)
})

context("async_reject")

test_that("async_reject", {

  is_even <- function(x) {
    force(x)
    delay(1/1000)$then(function(value) as.logical(! (x %% 2)))
  }

  do <- function() {
    d1 <- async_reject(1:10, is_even)$
      then(function(.) expect_identical(., c(1L, 3L, 5L, 7L, 9L)))

    d2 <- async_reject(numeric(), is_even)$
      then(function(.) expect_identical(., numeric()))

    d3 <- async_reject(1:10 * 2, is_even)$
      then(function(.) expect_identical(., numeric()))

    when_all(d1, d2, d3)
  }
  synchronise(do())
})

test_that("async_reject, errors", {

  called <- FALSE
  do <- function()  {
    async_reject(1:10, function(x) stop("doh"))$
      then(function() called <<- TRUE)$
      catch(error = function(e) {
        expect_equal(conditionMessage(e), "doh")
        expect_s3_class(e, "async_rejected")
      })
  }

  synchronise(do())
  expect_false(called)
})
