
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
  sync_wrap(dx <- fun())
  expect_true(is_deferred(dx))
})

test_that("begins asynchronously", {
  do <- async(function() {
    x <- 5
    foo <- async(function() x <<- 7)
    dx <- foo()$
      then(function() expect_equal(x, 7))
    expect_equal(x, 5)
    x <- 9
    await(dx)
    expect_equal(x, 7)
  })
  sync_wrap(do())
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

  sync_wrap(do())
})

test_that("resolves to the definition", {
  do <- async(function() {
    foo <- async(function () "blah")
    dx <- foo()$
      then(function(result) expect_equal(result, "blah"))
  })
  sync_wrap(do())
})

test_that("rejects with the thrown error", {
  do <- async(function() {
    act <- NULL
    exp <- simpleError("Expected thrown value to match rejection value")
    foo <- async(function() { stop(exp); "blah" })
    dx <- foo()$
      then(NULL, function(err) { act <<- exp; exp })$
      then(function(value) {
        if (is.null(act)) {
          stop("Extected function to throw")
        } else if (!identical(act, exp)) {
          stop(exp)
        }
      })
  })

  expect_silent(sync_wrap(do()))
})

test_that("works with await", {

  do <- async(function() {
    foo <- async(function() {
      await(delay(20/1000)$then(function(value) "blah"))
    })

    dx <- foo()$
      then(function(result) expect_equal(result, "blah"))
  })

  sync_wrap(do())
})

test_that("triggers error on unhandled rejection", {

  do <- async(function() {
    foo <- async(function() stop("Nobody handled me"))
    did_trigger <- FALSE

    tryCatch(
      await(foo()),
      error = function(e) did_trigger <<- TRUE
    )

    expect_true(did_trigger)
  })

  sync_wrap(do())
})
