
context("call_function")

test_that("nested event loops", {

  skip("fails currently")
  skip_on_cran()

  ## Create a function that finishes while its event loop is inactive
  sleeper <- function(x) { Sys.sleep(x); Sys.getpid() }
  afun1 <- async(function(x) { x; call_function(sleeper, args = list(x)) })
  afun2 <- async(function(x1, x2) {
    x1; x2
    p1 <- afun1(x1)
    p2 <- delay(0)$then(function() synchronise(afun1(x2)))
    when_all(p1, p2)
  })

  res <- synchronise(afun2(1, 2))
})

test_that("successful calls", {
  afun <- async(function(x) {
    when_all(
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid())
    )
  })

  res <- synchronise(afun())
  expect_true(is.integer(viapply(res, "[[", "result")))
})

test_that("calls that error", {
  afun <- async(function(x) {
    when_all(
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() stop("nope"))
    )
  })

  expect_error(synchronise(afun()), "nope")
})

test_that("calls that crash", {
  afun <- async(function(x) {
    when_all(
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() Sys.getpid()),
      call_function(function() utils::getFromNamespace("crash", "async")())
    )
  })

  expect_error(synchronise(afun()), "R session crashed with exit code")

  afun <- async(function(x) {
    when_all(
      call_function(function() utils::getFromNamespace("crash", "async")()),
      call_function(function() utils::getFromNamespace("crash", "async")()),
      call_function(function() utils::getFromNamespace("crash", "async")()),
      call_function(function() utils::getFromNamespace("crash", "async")())
    )
  })

  expect_error(synchronise(afun()), "R session crashed with exit code")
})
