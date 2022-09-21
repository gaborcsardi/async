
test_that("async_until", {

  count <- 1

  result <- synchronise(async_until(
    function() count == 5,
    function() {
      delay(1/1000)$then(function(value) count <<- count + 1)
    }
  ))

  expect_equal(count, 5)
  expect_equal(result, 5)
})

test_that("async_until is always called once", {

  called <- FALSE

  result <- synchronise(async_until(
    function() TRUE,
    function() {
      delay(1/1000)$then(function(value) called <<- TRUE)
    }
  ))

  expect_true(called)
  expect_true(result)
})

test_that("error", {

  do <- function() {
    async_until(
      function() i > 5,
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

test_that("test function throws", {

  called  <- FALSE

  do <- function() {
    async_until(
      function() stop("doh"),
      function() {
        delay(1/1000)$then(function(value) called <<- TRUE)
      }
    )
  }

  expect_error(synchronise(do()), "doh")
  expect_true(called)

  called <- FALSE
  do2 <- function() {
    do()$
      catch(error = function(e) expect_equal(conditionMessage(e), "doh"))
  }
  synchronise(do2())
  expect_true(called)
})
