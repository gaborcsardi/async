
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
