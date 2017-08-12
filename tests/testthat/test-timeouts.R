
context("timeouts")

test_that("timeout is called", {
  good <- FALSE
  ax <- set_timeout(function() good <<- TRUE, 1)

  await(ax)
  expect_true(good)
})
