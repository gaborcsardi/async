
context("async_constant")

test_that("creates a deferred value", {
  do <- function() {
    dx <- async_constant()
    expect_true(is_deferred(dx))
    dx
  }
  synchronise(do())

  do <- function() {
    dx <- async_constant("foobar")
    expect_true(is_deferred(dx))
    dx
  }
  synchronise(do())
})

test_that("resolves to the specified value", {
  do <- function() {
    async_constant("foobar")$
      then(function(x) expect_equal(x, "foobar"))
  }
  synchronise(do())

  do <- function() {
    async_constant()$
      then(function(x) expect_null(x))
  }
  synchronise(do())
})
