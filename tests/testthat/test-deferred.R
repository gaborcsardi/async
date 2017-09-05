
context("deferred")

test_that("error if not done yet", {
  dx <- delay(1/1000)
  expect_error(dx$get_value())
})

test_that("rejecting with a deferred", {
  x <- deferred$new(function(resolve, reject) {
    reject(delay(1/1000)$then(function(value) "OK"))
  })

  expect_equal(await(x), "OK")
})

test_that("action in formula notation", {
  dx <- deferred$new(~ resolve(TRUE))
  expect_true(await(dx))

  dx <- deferred$new(~ reject("oops"))
  expect_error(await(dx), "oops")

  dx <- deferred$new(~ if (TRUE) resolve(TRUE) else reject("oops"))
  expect_true(await(dx))

  dx <- deferred$new(~ if (FALSE) resolve(TRUE) else reject("oops"))
  expect_error(await(dx), "oops")
})

test_that("on_fulfilled / on_rejected without arguments", {
  dx <- deferred$new(~resolve(TRUE))$then(function() "OK")
  expect_equal(await(dx), "OK")

  dx <- deferred$new(~resolve(TRUE))$then(function() stop("oops"))
  expect_error(await(dx), "oops")

  dx <- deferred$new(~resolve(TRUE))$
    then(function() stop("ooops"))$
    catch(function() "aaah")
  expect_equal(await(dx), "aaah")
})
