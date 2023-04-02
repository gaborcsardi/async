
test_that("race() rejects (#76)", {
  defer_fail <- function() {
    deferred$new(action = function(resolve) stop("foo"))
  }

  expect_error(
    synchronise(async_race(
      delay(0.1),
      defer_fail()
    )),
    "foo"
  )
})
