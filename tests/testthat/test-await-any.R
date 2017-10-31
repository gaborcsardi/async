
context("await_any")

test_that("returns asap", {
  do <- async(function() {
    tic <- Sys.time()
    dx1 <- delay(1)$then(function(value) "foo")
    dx2 <- delay(1/10000)$then(function(value) "bar")

    res <- await_any(dx1, dx2)
    expect_true(Sys.time() - tic < as.difftime(1, unit = "secs"))
    expect_equal(res, "bar")
  })
  synchronise(do())
})

test_that("fails asap", {
  do <- async(function() {
    tic <- Sys.time()
    dx1 <- delay(1)$then(function(value) "foo")
    dx2 <- delay(1/10000)$then(function(value) stop("blah"))
    expect_error(await_any(dx1, dx2), "blah")
    expect_true(Sys.time() - tic < as.difftime(1, unit = "secs"))
  })
  synchronise(do())
})

test_that("non deferred resolves right away", {
  do <- async(function() {
    dx1 <- delay(1/100)$then(function(value) "foo")
    expect_equal(await_any("foo", dx1), "foo")
  })
  synchronise(do())
})
