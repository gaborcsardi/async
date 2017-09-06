
context("await_env")

test_that("empty env", {
  expect_equal(
    await_env(new.env()),
    structure(list(), names = character())
  )
})

test_that("only regular objects", {
  env <- new.env()
  env$foo <- "foo"
  env$bar <- 1:10
  expect_equal(
    await_env(env),
    as.list(env)
  )
})

test_that("deferred value in env", {
  env <- new.env()
  env$foo <- async_constant()
  env$bar <- delay(1/10000)
  l <- as.list(env)
  expect_equal(
    await_env(env),
    await_all(.list = l)
  )
})

test_that("dynamically change the number of deferred values", {
  env <- new.env()
  env$foo <- delay(1/10000)$
    then(function() {
      env$foo2 <- async_constant("OK2")
      "OK"
    })

  expect_equal(
    sort_by_name(await_env(env)),
    list(foo = "OK", foo2 = "OK2")
  )
})

test_that("remove a deferred value", {
  env <- new.env()
  env$foo <- delay(1)
  env$bar <- delay(1/10000)$
    then(function() {
      rm("foo", envir = env)
      "OK"
    })

  expect_equal(await_env(env), list(bar = "OK"))
})

test_that("error", {
  env <- new.env()
  env$foo <- delay(1/1000)$then(~ stop("ooops"))

  expect_error(await_env(env), "ooops")
})
