
context("async_some")

test_that("async_some", {

  is_odd <- function(x) {
    force(x)
    delay(1/1000)$then(function(value) as.logical(x %% 2))
  }

  result <- synchronise(async_some(1:10, is_odd))
  expect_identical(result, TRUE)

  result <- synchronise(async_some(numeric(), is_odd))
  expect_identical(result, FALSE)

  result <- synchronise(async_some(1:10 * 2, is_odd))
  expect_identical(result, FALSE)

})

test_that("async_some, errors", {

  called <- FALSE
  do <- function()  {
    async_some(1:10, function(x) stop("doh"))$
      then(function() called <<- TRUE)$
      catch(error = function(e) {
        expect_equal(conditionMessage(e), "doh")
        expect_s3_class(e, "async_rejected")
      })
  }

  synchronise(do())
  expect_false(called)
})
