
context("event loop")

test_that("create", {
  el <- event_loop$new()
  expect_s3_class(el, "event_loop")
})

test_that("next tick", {
  el <- event_loop$new()

  ticked <- FALSE
  error <- "foo"
  result <- "bar"
  el$add_next_tick(
    function() ticked <<- TRUE,
    function(err, res) { error <<- err; result <<- res }
  )
  el$run()

  expect_true(ticked)
  expect_null(error)
  expect_true(result)
})

test_that("next tick, simple error", {
  el <- event_loop$new()

  ticked <- FALSE
  error <- "foo"
  result <- "bar"
  el$add_next_tick(
    function() { ticked <<- TRUE; stop("ohno") },
    function(err, res) { error <<- err; result <<- res }
  )
  el$run()

  expect_true(ticked)
  expect_null(result)
  expect_s3_class(error, "async_event_loop_error")
  call <- conditionCall(error)
  expect_equal(call[[2]][[1]], quote(stop("ohno")))
  expect_equal(tail(call[[1]], 3)[[1]][[1]], quote(el_add_next_tick))
})

test_that("error stack on HTTP errors", {
  el <- event_loop$new()

  error <- "foo"
  result <- "bar"

  f <- function() g()
  g <- function() {
    handle <- new_handle(url = "http://0.42.42.42", timeout = 1)
    el$add_http(
      handle,
      function(err, res) { error <<- err; result <<- res }
    )
  }
  f()
  el$run()

  expect_null(result)
  expect_s3_class(error, c("async_http_error", "async_event_loop_error"))
  call <- conditionCall(error)
  expect_equal(tail(call[[1]], 6)[[1]][[1]], quote(f))
  expect_equal(tail(call[[1]], 6)[[2]][[1]], quote(g))
})

test_that("errors in embedded event loops", {
  ticked <- ticked2 <- ticked3 <- FALSE
  error  <- error2  <- error3  <- "foo"
  result <- result2 <- result3 <- "bar"

  f3 <- function() g3()
  g3 <- function() {
    el <- event_loop$new()
    el$add_next_tick(
      function() { ticked <<- TRUE; stop("ohno") },
      function(err, res) {
        error <<- err
        result <<- res
        if (!is.null(err)) stop(err)
      }
    )
    el$run()
  }

  f2 <- function() g2()
  g2 <- function() {
    el <- event_loop$new()
    el$add_next_tick(
      function() { ticked2 <<- TRUE; f3() },
      function(err, res) {
        error2 <<- err
        result2 <<- res
        if (!is.null(err)) stop(err)
      }
    )
    el$run()
  }

  mel <- event_loop$new()
  mel$add_next_tick(
    function() { ticked3 <<- TRUE; f2() },
    function(err, res) { error3 <<- err; result3 <<- res }
  )
  mel$run()

  expect_true(ticked)
  expect_true(ticked2)
  expect_true(ticked3)
  expect_s3_class(error, "async_event_loop_error")
  expect_s3_class(error2, "async_event_loop_error")
  expect_s3_class(error3, "async_event_loop_error")
  expect_null(result)
  expect_null(result2)
  expect_null(result3)
})
