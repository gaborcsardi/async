
context("async_until")

test_that("async_until", {

  count <- 1

  result <- wait_for(async_until(
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

  result <- wait_for(async_until(
    function() TRUE,
    function() {
      delay(1/1000)$then(function(value) called <<- TRUE)
    }
  ))

  expect_true(called)
  expect_true(result)
})

test_that("test function throws", {

  expect_error(
    wait_for(async_until(
      function() stop("doh"),
      function() {
        delay(1/1000)$then(function(value) called <<- TRUE)
      }
    )),
    "doh"
  )
})
