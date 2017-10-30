
context("longstack")

test_that("single deferred value (http)", {

  do <- async(function() {
    err <- NULL
    dx <- http_get("http://0.42.42.42", timeout = 1)
    cmon_not_this <- function() {
      err <<- tryCatch(await(dx), error = identity)
    }
    cmon_not_this()
    expect_s3_class(err, c("async_http_error", "async_deferred_rejected"))
    call <- err$call
    expect_false(is.null(
      find_call_in_stack(call$start, quote(http_get))
    ))
    expect_null(find_in_stack(call$start, quote(cmon_not_this())))
  })
  sync_wrap(do())

  do <- async(function() {
    but_yes_this <- function() http_get("http://0.42.42.42", timeout = 1)
    dx <- but_yes_this()
    err <- tryCatch(await(dx), error = identity)
    expect_s3_class(err, c("async_http_error", "async_deferred_rejected"))
    call <- err$call
    expect_false(is.null(
      find_in_stack(call$start, quote(but_yes_this()))
    ))
  })
  sync_wrap(do())
})

test_that("single async function", {

  afun <- async(function() stop("boo"))

  do <- async(function() {
    err <- NULL
    dx <- afun()
    cmon_not_this <- function() {
      err <<- tryCatch(await(dx), error = identity)
    }
    cmon_not_this()
    expect_equal(err$message, "boo")
    expect_s3_class(err, c("async_error", "async_deferred_rejected"))
    call <- err$call
    expect_false(is.null(
      find_call_in_stack(call$start, quote(afun))
    ))
    expect_null(find_in_stack(call$start, quote(cmon_not_this())))
  })
  sync_wrap(do())

  do <- async(function() {
    but_yes_this <- function() afun()
    dx <- but_yes_this()
    err <- tryCatch(await(dx), error = identity)
    expect_equal(err$message, "boo")
    expect_s3_class(err, c("async_error", "async_deferred_rejected"))
    call <- err$call
    expect_false(is.null(
      find_in_stack(call$start, quote(but_yes_this()))
    ))
    expect_false(is.null(
      find_in_stack(call$eval, quote(stop("boo")))
    ))
  })
  sync_wrap(do())
})

test_that("async function with a stack", {

  g <- function() stop("ohno")
  f <- function() g()
  afun <- async(function() f())

  do <- async(function() {
    err <- tryCatch(await(afun()), error = identity)
    call <- err$call
    expect_equal(err$message, "ohno")
    f_no <- find_in_stack(call$eval, quote(f()))
    g_no <- find_in_stack(call$eval, quote(g()))
    stop_no <- find_in_stack(call$eval, quote(stop("ohno")))
    expect_false(is.null(f_no))
    expect_false(is.null(g_no))
    expect_false(is.null(stop_no))
    expect_true(f_no == g_no - 1)
    expect_true(g_no == stop_no - 1)

    call2 <- trim_long_stack(call)
    expect_equal(tail(call2$start, 1)[[1]], quote(afun()))
  })
  sync_wrap(do())
})

test_that("then, parent stack", {
  g <- function() stop("ohno")
  f <- function() g()

  do <- async(function() {
    dx1 <- delay(1/1000)
    dx2 <- dx1$then(f)
    err <- tryCatch(await(dx2), error = identity)
    call <- err$call
    expect_false(is.null(
      find_in_stack(call$parent$start, quote(delay(1/1000)))
    ))
  })
  sync_wrap(do())
})
