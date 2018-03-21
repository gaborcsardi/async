
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

    all <- when_all(
      dx$catch(function(e) expect_match(e$message, "changed my mind")),
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(~ expect_equal(dx$get_state(), "rejected")),
      dx$catch(~ expect_true(cancelled)),
      dx$catch(~ expect_match(cancel_msg, "changed my mind"))
    )

    dx$cancel("changed my mind")

    all
  })

  synchronise(do())
})

test_that("then() is also rejected on cancel", {
  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    dx2 <- dx$then(function() "not this far")

    all <- when_all(
      dx2$catch(function(e) expect_match(e$message, "changed my mind")),
      dx$catch(function(e) expect_match(e$message, "changed my mind")),
      dx2$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx2$catch(~ expect_equal(dx2$get_state(), "rejected")),
      dx$catch(~ expect_equal(dx$get_state(), "rejected"))
    )

    dx$cancel("changed my mind")

    all
  })
  synchronise(do())
})

test_that("can catch and handle cancellation", {

  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    all <- when_all(
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(function(e) expect_match(e$message, "changed my mind"))
    )

    dx$cancel("changed my mind")

    all
  })
  synchronise(do())
})
