
context("sequence")

test_that("sequence", {

  skip("need to rewrite with deferred")  
  
  add1 <- function(n, callback) {
    force(n) ; force(callback)
    delay(10/1000, function() callback(NULL, n + 1))
  }
  mul3 <- function(n, callback) {
    force(n) ; force(callback)
    delay(10/1000, function() callback(NULL, n * 3))
  }

  add1mul3 <- sequence(add1, mul3)
  result <- NULL
  wait_for(add1mul3(4, callback = function (err, res) {
    result <<- res
  }))

  expect_equal(result, 15)
})

test_that("sequence, asyncify", {

  skip("need to rewrite with deferred")  
  
  add1 <- function(n, callback) {
    force(n) ; force(callback)
    delay(10/1000, function() callback(NULL, n + 1))
  }
  mul3 <- function(n, callback) {
    force(n) ; force(callback)
    delay(10/1000, function() callback(NULL, n * 3))
  }
  sub14 <- asyncify(function(n) n - 14)

  add1mul3sub14 <- sequence(add1, mul3, sub14)
  result <- NULL
  wait_for(add1mul3sub14(4, callback = function (err, res) {
    result <<- res
  }))

  expect_equal(result, 1)
})
