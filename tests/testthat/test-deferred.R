
context("deferred")

test_that("action in formula notation", {
  do <- function() {
    dx1 <- deferred$new(~ resolve(TRUE))$
      then(~ expect_true(.))

    dx2 <- deferred$new(~ stop("oops"))$
      catch(error = ~ expect_match(conditionMessage(.), "oops"))

    dx3 <- deferred$new(~ if (TRUE) resolve(TRUE) else stop("oops"))$
      then(~ expect_true(.))

    dx4 <- deferred$new(~ if (FALSE) resolve(TRUE) else stop("oops"))$
      catch(error = ~ expect_match(conditionMessage(.), "oops"))

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
      catch(error = ~ expect_match(conditionMessage(.), "oops"))

    dx3 <- deferred$new(~resolve(TRUE))$
      then(~ stop("ooops"))$
      catch(error = ~ "aaah")$
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
    d3 <- d2$then(~ expect_true(is.null(get_private(d2)$parent)))
    expect_equal(length(get_private(d2)$parent), 0)
    d3
  }
  synchronise(do())
})

test_that("unused computation is never created", {
  called1 <- called2 <- FALSE
  do <- function() {
    d1 <- deferred$new(
      function(resolve, reject) { called1 <<- TRUE; resolve("foo") })
    d2 <- deferred$new(
      function(resolve, reject) { called2 <<- TRUE; resolve("bar") })
    d2
  }
  expect_equal(synchronise(do()), "bar")
  expect_false(called1)
  expect_true(called2)
})

test_that("replacing promises does not leak", {

  loop <- function(limit = 5) {
    limit
    n <- 1
    x <- list()

    do <- function() {
      x <<- append(x, list(async_list()))
      if (n < limit) {
        n <<- n + 1
        delay(0)$then(do)
      } else {
        async_constant(x)
      }
    }

    do()
  }

  x <- synchronise(when_any(loop()))
  expect_equal(length(x), 5L)
  expect_equal(nrow(x[[2]]), nrow(x[[5]]))
})
