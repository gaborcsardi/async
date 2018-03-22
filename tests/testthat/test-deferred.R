
context("deferred")

test_that("error if not done yet", {
  do <- async(function() {
    dx <- delay(1/1000)
    expect_error(private(dx)$get_value())
    dx
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
  do <- function() {
    dx1 <- deferred$new(~ resolve(TRUE))$
      then(~ expect_true(.))

    dx2 <- deferred$new(~ reject("oops"))$
      catch(~ expect_match(., "oops"))

    dx3 <- deferred$new(~ if (TRUE) resolve(TRUE) else reject("oops"))$
      then(~ expect_true(.))

    dx4 <- deferred$new(~ if (FALSE) resolve(TRUE) else reject("oops"))$
      catch(~ expect_match(., "oops"))

    when_all(dx1, dx2, dx3, dx4)
  }
  synchronise(do())
})

test_that("on_fulfilled / on_rejected without arguments", {
  do <- async(function() {
    dx1 <- deferred$new(~resolve(TRUE))$
      then(~ "OK")$
      then(~ expect_equal(., "OK"))

    dx2 <- deferred$new(~resolve(TRUE))$
      then(~ stop("oops"))$
      catch(~ expect_match(conditionMessage(.), "oops"))

    dx3 <- deferred$new(~resolve(TRUE))$
      then(~ stop("ooops"))$
      catch(~ "aaah")$
      then(~ expect_equal(., "aaah"))

    when_all(dx1, dx2, dx3)
  })
  synchronise(do())
})

test_that("parent pointer", {

  ## Parent pointer is added when the deferred is created, but it
  ## is removed, once the promise is resolved
  do <- function() {
    d1 <- delay(1/1000)
    d2 <- d1$then(force)
    d3 <- d2$then(~ expect_true(is.null(private(d2)$parent)))
    expect_false(is.null(private(d2)$parent))
    when_all(d1, d2, d3)
  }
  synchronise(do())
})
