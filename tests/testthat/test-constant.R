
context("async_constant")

test_that("creates a deferred value", {
  dx <- async_constant()
  expect_true(is_deferred(dx))
  wait_for(dx)

  dx <- async_constant("foobar")
  expect_true(is_deferred(dx))
  wait_for(dx)
})

test_that("resolves to the specified value", {
  dx <- async_constant()
  expect_null(wait_for(dx))

  dx <- async_constant("foobar")
  expect_equal(wait_for(dx), "foobar")
})
