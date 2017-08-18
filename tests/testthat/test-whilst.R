
context("whilst")

test_that("whilst", {

  count <- 0
  nn <- NULL

  wait_for(whilst(
    function() count < 5,
    function(callback) {
      count <<- count + 1
      callback(NULL, count)
    },
    function (err, n) {
      nn <<- n
    }
  ))

  expect_false(is.null(nn))
  expect_equal(nn, 5)
})

test_that("whilst with false test", {

  result <- NULL

  expect_silent({
    wait_for(whilst(
      function() FALSE,
      function(callback) {
        stop("Not reached")
        callback(NULL, "Not here")
      },
      function(err, res) {
        result <<- res
      }
    ))
  })

  expect_null(result)
})

test_that("error", {

  i <- 1
  error <- result <- NULL
  wait_for(whilst(
    function() i < 5,
    function(callback) {
      i <<- i + 1
      if (i < 3) {
        callback(NULL, "Good so far")
      } else {
        callback("This is bad", NULL)
      }
    },
    function(err, res) { error <<- err; result <<- res }
  ))

  expect_null(result)
  expect_equal(error, "This is bad")
})

test_that("whilst, asyncify", {

  count <- 0
  nn <- NULL

  wait_for(whilst(
    function() count < 5,
    asyncify(function() count <<- count + 1),
    function (err, n) nn <<- n
  ))

  expect_false(is.null(nn))
  expect_equal(nn, 5)
})
