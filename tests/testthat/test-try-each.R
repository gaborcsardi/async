
context("async_try_each")

test_that("only one, success", {
  do <- function() {
    async_try_each(
      async(function() "cool")()
    )
  }
  expect_equal(synchronise(do()), "cool")
})

test_that("only one, fail", {
  err <- NULL
  do <- function() {
    async_try_each(
      async(function() stop("doh"))()
    )$catch(error = function(e) err <<- e)
  }
  synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_equal(conditionMessage(err), "async_try_each failed")
  expect_equal(conditionMessage(err$errors[[1]]), "doh")
})

test_that("first success", {
  do <- function() {
    async_try_each(
      async(function() "cool")(),
      async(function() "cool2")(),
      async(function() stop("doh"))()
    )
  }
  expect_equal(synchronise(do()), "cool")
})

test_that("second success", {
  do <- function() {
    async_try_each(
      async(function() stop("doh"))(),
      async(function() "cool")(),
      async(function() stop("doh2"))(),
      async(function() "cool2")()
    )
  }
  expect_equal(synchronise(do()), "cool")
})

test_that("empty", {
  do <- function() {
    async_try_each()
  }
  expect_null(synchronise(do()))
})

test_that("all fail", {
  err <- NULL
  do <- function() {
    async_try_each(
      async(function() stop("doh"))(),
      async(function() stop("doh2"))(),
      async(function() stop("doh3"))()
    )$catch(error = function(e) err <<- e)
  }
  synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_equal(conditionMessage(err), "async_try_each failed")
  expect_equal(conditionMessage(err$errors[[1]]), "doh")
  expect_equal(conditionMessage(err$errors[[2]]), "doh2")
  expect_equal(conditionMessage(err$errors[[3]]), "doh3")
})
