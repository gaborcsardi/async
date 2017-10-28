
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
  expect_s3_class(error, "async_error")
  expect_equal(error$stack[[2]][[1]], quote(stop("ohno")))
  expect_equal(tail(error$stack[[1]], 3)[[1]][[1]], quote(el_add_next_tick))
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
  expect_s3_class(error, c("async_http_error", "async_error"))
  expect_equal(tail(error$stack[[1]], 6)[[1]][[1]], quote(f))
  expect_equal(tail(error$stack[[1]], 6)[[2]][[1]], quote(g))
})
