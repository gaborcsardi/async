
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
