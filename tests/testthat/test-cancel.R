
context("cancellation")

test_that("on_cancel callback is called", {
  do <- async(function() {
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
    expect_error(await(dx), "changed my mind")
    expect_error(await(dx), class = "async_cancelled")

    expect_equal(dx$get_state(), "rejected")
    expect_true(cancelled)
    expect_match(cancel_msg, "changed my mind")
  })

  synchronise(do())
})

test_that("then() is also rejected on cancel", {
  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    dx2 <- dx$then(function() "not this far")

    dx$cancel("changed my mind")
    expect_error(await(dx2), "changed my mind")
    expect_error(await(dx), "changed my mind")
    expect_error(await(dx2), class = "async_cancelled")
    expect_error(await(dx), class = "async_cancelled")
    expect_equal(dx2$get_state(), "rejected")
    expect_equal(dx$get_state(), "rejected")
  })
  synchronise(do())
})

test_that("can catch and handle cancellation", {

  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    dx$cancel("changed my mind")
    expect_error(await(dx), "changed my mind")
    expect_error(await(dx), class = "async_cancelled")
  })
  synchronise(do())
})
