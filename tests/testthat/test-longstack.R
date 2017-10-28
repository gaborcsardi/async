
context("longstack")

test_that("single deferred value (http)", {

  err <- NULL
  dx <- http_get("http://0.42.42.42", timeout = 1)
  cmon_not_this <- function() {
    err <<- tryCatch(await(dx), error = identity)
  }
  cmon_not_this()
  expect_s3_class(err, c("async_http_error", "async_deferred_rejected"))
  call <- conditionCall(err)
  expect_false(is.null(
    find_call_in_stack(call$mystart, quote(http_get))
  ))
  expect_null(find_in_stack(call$mystart, quote(cmon_not_this())))

  but_yes_this <- function() http_get("http://0.42.42.42", timeout = 1)
  dx <- but_yes_this()
  err <- tryCatch(await(dx), error = identity)
  expect_s3_class(err, c("async_http_error", "async_deferred_rejected"))
  call <- conditionCall(err)
  expect_false(is.null(
    find_in_stack(call$mystart, quote(but_yes_this()))
  ))
})

test_that("single async function", {

  err <- NULL
  afun <- async(function() stop("boo"))
  dx <- afun()
  cmon_not_this <- function() {
    err <<- tryCatch(await(dx), error = identity)
  }
  cmon_not_this()
  expect_equal(conditionMessage(err), "boo")
  expect_s3_class(err, c("async_error", "async_deferred_rejected"))
  call <- conditionCall(err)
  expect_false(is.null(
    find_call_in_stack(call$mystart, quote(afun))
  ))
  expect_null(find_in_stack(call$mystart, quote(cmon_not_this())))

  but_yes_this <- function() afun()
  dx <- but_yes_this()
  err <- tryCatch(await(dx), error = identity)
  expect_equal(conditionMessage(err), "boo")
  expect_s3_class(err, c("async_error", "async_deferred_rejected"))
  call <- conditionCall(err)
  expect_false(is.null(
    find_in_stack(call$mystart, quote(but_yes_this()))
  ))
  expect_false(is.null(
    find_in_stack(call$myeval, quote(stop("boo")))
  ))
})

test_that("async function with a stack", {

  g <- function() stop("ohno")
  f <- function() g()
  afun <- async(function() f())

  err <- tryCatch(await(afun()), error = identity)
  expect_equal(conditionMessage(err), "ohno")


})
