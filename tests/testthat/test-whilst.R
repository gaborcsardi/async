
context("async_whilst")

test_that("async_whilst", {

  count <- 0
  result <- NULL

  result <- synchronise(async_whilst(
    function(...) count < 5,
    function() {
      delay(1/1000)$then(function(value) count <<- count + 1)
    }
  ))

  expect_equal(result, 5)
})

test_that("async_whilst with false test", {

  result <- NULL

  expect_silent({
    synchronise(async_whilst(
      function() FALSE,
      function() {
        delay(1/1000)$then(function(value) stop("Not reached"))
      }
    ))
  })

  expect_null(result)
})

test_that("error", {

  do <- function() {
    async_whilst(
      function() i < 5,
      function() delay(1/1000)$then(function(value) {
        i <<- i + 1
        if (i >= 3) stop("doh")
      })
    )
  }

  i <- 1
  expect_error(synchronise(do()), "doh")

  i <- 1
  do2 <- function() {
    do()$
      catch(error = function(e) expect_equal(conditionMessage(e), "doh"))
  }
  synchronise(do2())
})

test_that("test throws", {

  called  <- FALSE

  do <- function() {
    async_whilst(
      function() stop("doh"),
      function() {
        delay(1/1000)$then(function(value) called <<- TRUE)
      }
    )
  }

  expect_error(synchronise(do()), "doh")
  expect_false(called)

  called <- FALSE
  do2 <- function() {
    do()$
      catch(error = function(e) expect_equal(conditionMessage(e), "doh"))
  }
  synchronise(do2())
  expect_false(called)
})
