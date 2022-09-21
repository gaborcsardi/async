
test_that("async_detect", {

  is_odd <- function(x) {
    force(x)
    delay(1/1000)$
      then(function(value) as.logical(x %% 2))
  }

  test <- function(limit) {
    d1 <- async_detect(1:10, is_odd, .limit = limit)$
      then(function(.) expect_true(. %in% c(1L, 3L, 5L, 7L, 9L)))

    d2 <- async_detect(2:10, is_odd, .limit = limit)$
      then(function(.) expect_true(. %in% c(3L, 5L, 7L, 9L)))

    d3 <- async_detect(2, is_odd, .limit = limit)$
      then(function(.) expect_null(.))

    d4 <- async_detect(c(1:10 * 2L, 43L), is_odd, .limit = limit)$
      then(function(.) expect_identical(., 43L))

    d5 <- async_detect(numeric(), is_odd, .limit = limit)$
      then(function(.) expect_null(.))

    d6 <- async_detect(1:10 * 2, is_odd, .limit = limit)$
      then(function(.) expect_null(.))

    when_all(d1, d2, d3, d4, d5, d6)
  }

  lapply(c(Inf, 1, 2, 3, 5, 10, 20), function(x) synchronise(test(x)))
})

test_that("async_detect errors", {
  called <- FALSE
  do <- function()  {
    async_detect(1:10, function(x) stop("doh"))$
      then(function() called <<- TRUE)$
      catch(error = function(e) {
        expect_equal(conditionMessage(e), "doh")
        expect_s3_class(e, "async_rejected")
      })
  }

  synchronise(do())
  expect_false(called)
})
