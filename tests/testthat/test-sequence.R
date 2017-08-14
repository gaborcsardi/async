
context("sequence")

test_that("sequence", {

  add1 <- function(n, callback) {
    force(n) ; force(callback)
    set_timeout(function() callback(NULL, n + 1), 10/1000)
  }
  mul3 <- function(n, callback) {
    force(n) ; force(callback)
    set_timeout(function() callback(NULL, n * 3), 10/1000)
  }

  add1mul3 <- sequence(add1, mul3)
  result <- NULL
  await(add1mul3(4, callback = function (err, res) {
    result <<- res
  }))

  expect_equal(result, 15)
})
