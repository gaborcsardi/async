
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

return()

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
      call_function(function() asNamespace("callr")$crash())
    )
  })

  expect_error(synchronise(afun()), "R session crashed with exit code")

  afun <- async(function(x) {
    when_all(
      call_function(function() asNamespace("callr")$crash()),
      call_function(function() asNamespace("callr")$crash()),
      call_function(function() asNamespace("callr")$crash()),
      call_function(function() asNamespace("callr")$crash())
    )
  })

  ## Either might happen here
  err <- tryCatch(synchronise(afun()), error = function(x) x)
  expect_true(
    grepl("R session crashed with exit code", err$message) ||
    grepl("R session closed the process connection", err$message))
})

test_that("handling call errors", {

  worker_pid <- async(function() {
    call_function(function() Sys.getpid())$then(function(x) x$result)
  })

  afun <- async(function(x) {
    when_all(
      worker_pid(),
      worker_pid(),
      worker_pid(),
      call_function(function() stop("nope"))$
        catch(error = function(e) e)
    )
  })

  res <- synchronise(afun())
  expect_true(is_count(res[[1]]))
  expect_true(is_count(res[[2]]))
  expect_true(is_count(res[[3]]))
  expect_s3_class(res[[4]], "async_rejected")
  expect_equal(res[[4]]$message, "nope")
})

test_that("mix calls with others", {

  skip_on_cran()
  skip_on_os("windows")
  skip_if_offline()

  afun <- async(function() {
    when_all(
      delay = delay(1/1000)$
        then(function() 1),
      http = http_get("https://eu.httpbin.org/status/418")$
        then(function(x) x$status_code),
      process = run_process("pwd")$
        then(function(x) str_trim(x$stdout)),
      r_process = run_r_process(function() 2)$
        then(function(x) x$result),
      call = call_function(function() 3)$
        then(function(x) x$result)
    )
  })

  res <- synchronise(afun())

  expect_equal(
    res,
    list(delay = 1,
         http = 418,
         process = getwd(),
         r_process = 2,
         call = 3)
  )
})
