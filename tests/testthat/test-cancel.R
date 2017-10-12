
context("cancellation")

test_that("on_cancel callback is called", {
  cancelled <- FALSE
  cancel_msg <- NULL
  dx <- deferred$new(
    action = function(resolve, reject) { },
    on_cancel = function(msg) {
      cancelled <<- TRUE
      cancel_msg <<- msg
    }
  )

  dx$cancel("changed my mind")
  expect_error(wait_for(dx), "changed my mind")
  expect_error(wait_for(dx), class = "async_cancelled")

  expect_equal(dx$get_state(), "rejected")
  expect_true(cancelled)
  expect_equal(cancel_msg, "changed my mind")
})

test_that("then() is also rejected on cancel", {
  dx <- deferred$new(action = function(resolve, reject) { })
  dx2 <- dx$then(function() "not this far")

  dx$cancel("changed my mind")
  expect_error(wait_for(dx2), "changed my mind")
  expect_error(wait_for(dx), "changed my mind")
  expect_error(wait_for(dx2), class = "async_cancelled")
  expect_error(wait_for(dx), class = "async_cancelled")
  expect_equal(dx2$get_state(), "rejected")
  expect_equal(dx$get_state(), "rejected")
})

test_that("can catch and handle cancellation", {

  dx <- deferred$new(action = function(resolve, reject) { })
  dx$cancel("changed my mind")
  expect_error(wait_for(dx), "changed my mind")
  expect_error(wait_for(dx), class = "async_cancelled")
})
