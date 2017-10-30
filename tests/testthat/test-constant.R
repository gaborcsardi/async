
context("async_constant")

test_that("creates a deferred value", {
  do <- async(function() {
    dx <- async_constant()
    expect_true(is_deferred(dx))
    await(dx)

    dx <- async_constant("foobar")
    expect_true(is_deferred(dx))
    await(dx)
  })
  sync_wrap(do())
})

test_that("resolves to the specified value", {
  do <- async(function() {
    dx <- async_constant()
    expect_null(await(dx))

    dx <- async_constant("foobar")
    expect_equal(await(dx), "foobar")
  })
  sync_wrap(do())
})
