
context("await_any")

test_that("returns asap", {
  tic <- Sys.time()
  dx1 <- delay(1)$then(function(value) "foo")
  dx2 <- delay(1/10000)$then(function(value) "bar")

  res <- await_any(dx1, dx2)
  expect_true(Sys.time() - tic < as.difftime(1, unit = "secs"))
  expect_equal(res, "bar")
})

test_that("fails asap", {
  dx1 <- delay(1)$then(function(value) "foo")
  dx2 <- delay(1/10000)$then(function(value) stop("blah"))
  expect_error(await_any(dx1, dx2), "blah")
})
