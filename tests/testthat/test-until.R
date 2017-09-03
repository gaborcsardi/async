
context("until")

test_that("until", {

  count <- 1

  result <- await(until(
    function() count == 5,
    function() {
      delay(1/1000)$then(function(value) count <<- count + 1)
    }
  ))

  expect_equal(count, 5)
  expect_equal(result, 5)
})

test_that("until is always called once", {

  called <- FALSE

  result <- await(until(
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
    await(until(
      function() stop("doh"),
      function() {
        delay(1/1000)$then(function(value) called <<- TRUE)
      }
    )),
    "doh"
  )
})
