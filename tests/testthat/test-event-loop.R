
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

test_that("event loop with only timers sleeps", {
  tim <- system.time(synchronise(delay(1/2)))
  expect_true(tim[[1]] + tim[[2]] < 0.4)
  expect_true(tim[[3]] >= 0.4)
})
