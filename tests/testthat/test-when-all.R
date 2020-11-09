
context("when_all")

test_that("when_all", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) "foo")
    d2 <- delay(1/1000)$then(function(value) "bar")

    when_all(d1, d2)$
      then(function(value) {
        done <<- TRUE
        value
      })
  })
  expect_equal(synchronise(do()), list("foo", "bar"))
  expect_true(done)
})

test_that("when_all, non-deferred", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) "foo")
    d2 <- "bar"

    when_all(d1, d2)$
      then(function(value) {
        done <<- TRUE
        expect_equal(value, list("foo", "bar"))
      })
  })
  synchronise(do())
  expect_true(done)
})

test_that("when_all, non-deferred only", {
  done <- FALSE
  do <- function() {
    d1 <- "foo"
    d2 <- "bar"

    when_all(d1, d2)$
      then(function(value) {
        done <<- TRUE
        expect_equal(value, list("foo", "bar"))
      })
  }
  synchronise(do())
  expect_true(done)
})

test_that("when_all, empty list", {
  done <- FALSE
  do <- async(function() {
    when_all()$
      then(function(value) {
        done <<- TRUE
        expect_equal(value, list())
      })
  })
  synchronise(do())
  expect_true(done)
})

test_that("when_all, error", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1/1000)$then(function(value) stop("foo"))
    d2 <- delay(1/1000)$then(function(value) "bar")

    when_all(d1, d2)$
      catch(error = function(reason) {
        done <<-  TRUE
        expect_match(reason$message, "foo")
      })
  })
  synchronise(do())
  expect_true(done)
})

test_that("when_all, multiple errors", {
  done <- FALSE
  err <- NULL
  do <- async(function() {
    d1 <- delay(2)$then(function(value) stop("foo"))
    d2 <- delay(1/10000)$then(function(value) stop("bar"))

    dx <- when_all(d1, d2)$
      catch(error = function(reason) {
        done <<- TRUE
        err <<- reason
      })
  })
  synchronise(do())
  expect_true(done)
  expect_match(conditionMessage(err), "bar")
})

test_that("resolving to NULL", {
  do <- async(function() {
    when_all(
      delay(0)$then(~ NULL),
      delay(0)$then(~ 46)
    )
  })

  ret <- synchronise(do())
  expect_equal(ret, list(NULL, 46))
})
