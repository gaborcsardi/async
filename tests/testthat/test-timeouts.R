
context("timeouts")

test_that("timeout is called", {
  good <- FALSE
  ax <- set_timeout(1/10, function() good <<- TRUE)

  wait_for(ax)
  expect_true(good)
})
