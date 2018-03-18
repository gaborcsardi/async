
context("async_constant")

test_that("creates a deferred value", {
  do <- async(function() {
    dx <- async_constant()
    expect_true(is_deferred(dx))
    dx <- async_constant("foobar")
    expect_true(is_deferred(dx))
  })
  synchronise(do())
})

test_that("resolves to the specified value", {
  do <- async(function() {
    dx <- async_constant()
    dx$then(function(x) expect_null(x))

    dx <- async_constant("foobar")
    dx$then(function(x) expect_equal(x, "foobar"))
  })
  synchronise(do())
})
