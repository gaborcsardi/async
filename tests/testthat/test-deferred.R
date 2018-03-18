
context("deferred")

test_that("error if not done yet", {
  do <- async(function() {
    dx <- delay(1/1000)
    expect_error(dx$get_value())
  })
  synchronise(do())
})

test_that("rejecting with a deferred", {
  do <- async(function() {
    deferred$new(function(resolve, reject) {
      reject(delay(1/1000)$then(function(value) "OK"))
    })
  })
  res <- synchronise(do())
  expect_equal(res, "OK")
})

test_that("action in formula notation", {
  do <- async(function() {
    dx <- deferred$new(~ resolve(TRUE))$
      then(~ expect_true(.))

    dx <- deferred$new(~ reject("oops"))$
      then(~ expect_error(., "oops"))

    dx <- deferred$new(~ if (TRUE) resolve(TRUE) else reject("oops"))$
      then(~ expect_true(.))

    dx <- deferred$new(~ if (FALSE) resolve(TRUE) else reject("oops"))$
      catch(~ expect_match(., "oops"))
  })
  synchronise(do())
})

test_that("on_fulfilled / on_rejected without arguments", {
  do <- async(function() {
    dx <- deferred$new(~resolve(TRUE))$
      then(~ "OK")$
      then(~ expect_equal(., "OK"))

    dx <- deferred$new(~resolve(TRUE))$
      then(~ stop("oops"))$
      catch(~ expect_equal(., "oops"))

    dx <- deferred$new(~resolve(TRUE))$
      then(~ stop("ooops"))$
      catch(~ "aaah")$
      then(~ expect_equal(., "aaah"))
  })
  synchronise(do())
})
