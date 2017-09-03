
context("timeout")

test_that("timed out", {
  f <- function() delay(1/10)$then(function(value) "OK")
  expect_error(await(timeout(f, 1/1000)), "Timed out")
})

test_that("did not time out", {
  f <- function() delay(1/1000)$then(function(value) "OK")
  expect_equal(await(timeout(f, 1/100)), "OK")
})

test_that("error before timeout", {
  f <- function() delay(1/1000)$then(function(value) stop("oops"))
  expect_error(await(timeout(f, 1/10)), "oops")
})

test_that("error after timeout", {
  f <- function() delay(1/10)$then(function(value) stop("oops"))
  expect_error(await(timeout(f, 1/1000)), "Timed out")
})
