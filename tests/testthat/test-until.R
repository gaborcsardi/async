
context("until")

test_that("until", {

  count <- 1
  result <- NULL

  until(
    function() count == 5,
    function(cb) {
      count <<- count + 1
      cb(NULL, count)
    },
    function(err, res) {
      result <<- res
    }
  )

  expect_equal(count, 5)
  expect_equal(result, 5)
})

test_that("until is always called once", {

  called <- FALSE
  result <- NULL

  until(
    function() TRUE,
    function(cb) {
      called <<- TRUE
      cb(NULL, called)
    },
    function(err, res) {
      result <<- res
    }
  )

  expect_true(called)
  expect_true(result)
})
