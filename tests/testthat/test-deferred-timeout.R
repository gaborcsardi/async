
context("deferred timeout")

test_that("deferred timeout", {
  good <- FALSE
  tic <- Sys.time()
  await(set_timeout(1/4)$then(function(value) good <<- TRUE))
  expect_true(Sys.time() - tic > as.difftime(1/4, unit = "secs"))
  expect_true(good)
})
