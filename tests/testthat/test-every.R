
context("async_every")

test_that("async_every", {

  is_odd <- function(x) {
    force(x)
    delay(1/1000)$then(function(value) as.logical(x %% 2))
  }

  do <- function() {
    d1 <- async_every(1:10, is_odd)$
      then(~ expect_identical(., FALSE))

    d2 <- async_every(numeric(), is_odd)$
      then(~ expect_identical(., TRUE))

    d3 <- async_every(1:10 * 2 + 1, is_odd)$
      then(~ expect_identical(., TRUE))

    when_all(d1, d2, d3)
  }
  synchronise(do())
})

test_that("async_every, errors", {

  called <- FALSE
  do <- function()  {
    async_every(1:10, function(x) stop("doh"))$
      then(function() called <<- TRUE)$
      catch(error = function(e) {
        expect_equal(conditionMessage(e), "doh")
        expect_s3_class(e, "async_rejected")
      })
  }

  synchronise(do())
  expect_false(called)
})
