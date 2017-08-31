
context("errors")

test_that("rejection", {

  dx <- set_timeout(1/10000)$
    then(function(value) stop("ohno!"))

  expect_error(await(dx, "ohno!"))
})

test_that("error propagates", {
  called <- FALSE
  dx <- set_timeout(1/10000)$
    then(function(x) x)$
    then(function(x) stop("ohno!"))$
    then(function(x) called <<- TRUE)

  expect_error(await(dx, "ohno!"))
  expect_false(called)
})

test_that("handled error is not an error any more", {

  dx <- set_timeout(1/10000)$
    then(function(x) stop("ohno!"))$
    then(NULL, function(x) "OK")

  expect_silent(await(dx))
  expect_equal(await(dx), "OK")
})
