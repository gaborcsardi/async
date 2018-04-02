
context("async")

test_that("it returns a function", {
  ret <- async(function() { })
  expect_true(is.function(ret))
})

test_that("it returns a function with the same arity", {
  defs <- list(
    function(a, b, c) { },
    function() { },
    function(a, b, c, d, e, f, g, h) { },
    function(x) { },
    function(a = "foo", b = "bar") { },
    function(...) { },
    function(x = 10, ..., last = "foo") { }
  )

  for (f in defs) {
    f2 <- async(f)
    expect_equal(formals(f), formals(f2))
  }
})

test_that("when called it returns a deferred", {
  fun <- async(function() "foo")
  synchronise(dx <- fun())
  expect_true(is_deferred(dx))
})

test_that("preserves closure", {
  env <- new.env()
  foo <- local(envir = env, {
    baz <- list(x = 7)
    async(function() parent.env(environment()))
  })

  do <- async(function() {
    dx <- foo()$
      then(function(result) expect_identical(result, env))
  })

  synchronise(do())
})

test_that("resolves to the definition", {
  do <- async(function() {
    foo <- async(function () "blah")
    dx <- foo()$
      then(function(result) expect_equal(result, "blah"))
  })
  synchronise(do())
})

test_that("rejects with the thrown error", {
  do <- async(function() {
    act <- NULL
    exp <- simpleError("Expected thrown value to match rejection value")
    foo <- async(function() { stop(exp); "blah" })
    dx <- foo()$
      catch(error = function(err) { act <<- exp; exp })$
      then(function(value) {
        if (is.null(act)) {
          stop("Extected function to throw")
        } else if (!identical(act, exp)) {
          stop(exp)
        }
      })
  })

  expect_silent(synchronise(do()))
})

test_that("triggers error on unhandled rejection", {

  did_trigger <- FALSE
  do <- async(function() {
    foo <- async(function() stop("Nobody handled me"))
    foo()
  })

  tryCatch(
    synchronise(do()),
    error = function(e) did_trigger <<- TRUE
  )
  expect_true(did_trigger)
})

test_that("can be cancelled", {

  called <- called2 <- FALSE
  do <- function() {
    afun <- async(function() called <<- TRUE)
    dx <- afun()
    dy <- dx$then(function() called2 <<- TRUE)
    dx$cancel()
    dy
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_equal(conditionMessage(err), "Cancelled")
  expect_s3_class(err, "async_cancelled")
  expect_s3_class(err, "async_rejected")
  expect_false(called)
  expect_false(called2)
})

test_that("built-ins are marked as async", {
  expect_true(is_async(async_constant))
  expect_true(is_async(async_detect))
  expect_true(is_async(async_every))
  expect_true(is_async(async_filter))
  expect_true(is_async(async_map))
  expect_true(is_async(async_reflect))
  expect_true(is_async(async_retry))
  expect_true(is_async(async_sequence))
  expect_true(is_async(async_some))
  expect_true(is_async(async_timeout))
  expect_true(is_async(async_until))
  expect_true(is_async(async_whilst))
  expect_true(is_async(delay))
  expect_true(is_async(http_get))
  expect_true(is_async(http_head))
  expect_true(is_async(when_all))
  expect_true(is_async(when_any))
  expect_true(is_async(when_some))
})
