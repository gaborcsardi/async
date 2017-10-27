
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
