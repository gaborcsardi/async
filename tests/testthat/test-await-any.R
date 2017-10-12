
context("wait_for_any")

test_that("returns asap", {
  tic <- Sys.time()
  dx1 <- delay(1)$then(function(value) "foo")
  dx2 <- delay(1/10000)$then(function(value) "bar")

  res <- wait_for_any(dx1, dx2)
  expect_true(Sys.time() - tic < as.difftime(1, unit = "secs"))
  expect_equal(res, "bar")
})

test_that("fails asap", {
  tic <- Sys.time()
  dx1 <- delay(1)$then(function(value) "foo")
  dx2 <- delay(1/10000)$then(function(value) stop("blah"))
  expect_error(wait_for_any(dx1, dx2), "blah")
  expect_true(Sys.time() - tic < as.difftime(1, unit = "secs"))
})

test_that("non deferred resolves right away", {
  dx1 <- delay(1/100)$then(function(value) "foo")
  expect_equal(wait_for_any("foo", dx1), "foo")
})
