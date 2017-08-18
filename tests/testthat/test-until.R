
context("until")

test_that("until", {

  count <- 1
  result <- NULL

  await(until(
    function() count == 5,
    function(callback) {
      count <<- count + 1
      callback(NULL, count)
    },
    function(err, res) {
      result <<- res
    }
  ))

  expect_equal(count, 5)
  expect_equal(result, 5)
})

test_that("until is always called once", {

  called <- FALSE
  result <- NULL

  await(until(
    function() TRUE,
    function(callback) {
      called <<- TRUE
      callback(NULL, called)
    },
    function(err, res) {
      result <<- res
    }
  ))

  expect_true(called)
  expect_true(result)
})

test_that("until, asyncify", {

  count <- 1
  result <- NULL

  await(until(
    function() count == 5,
    asyncify(function() { count <<- count + 1; count }),
    function(err, res) result <<- res
  ))

  expect_equal(count, 5)
  expect_equal(result, 5)
})
