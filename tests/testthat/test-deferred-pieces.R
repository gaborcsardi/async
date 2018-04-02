
context("deferred pieces")

test_that("def__make_parent_*", {
  good <- list(
    NULL,
    function(x) x,
    ~ .,
    ~ .x,
    function() 42,
    function(value, resolve) resolve(value),
    function(value, resolve, id) resolve(value)
  )

  bad <- list(
    123,
    function(a, b, c) resolve(value)
  )

  eta <- function(value, resolve, id) { }
  
  for (f in good) {
    res <- def__make_parent_resolve(f)
    expect_equal(formals(res), formals(eta))
    res2 <- def__make_parent_reject(f)
    expect_equal(formals(res2), formals(eta))
  }

  for (f in bad) {
    expect_error(def__make_parent_resolve(f))
    expect_error(def__make_parent_reject(f))
  }
})

test_that("def__make_parent_resolve", {
  ## NULL
  r1 <- def__make_parent_resolve(NULL)
  res <- NULL
  val <- NULL
  r1(42,
     function(x) { res <<- "resolve"; val <<- x },
     1)
  expect_equal(res, "resolve")
  expect_equal(val, 42)

  ## function without args
  r2 <-  def__make_parent_resolve(function() 42 * 42)
  res <- NULL
  val <- NULL
  r2(42,
     function(x) { res <<- "resolve"; val <<- x },
     1)
  expect_equal(res, "resolve")
  expect_equal(val, 42 * 42)

  ## function with value arg
  r2 <-  def__make_parent_resolve(function(val) val)
  res <- NULL
  val <- NULL
  r2(42,
     function(x) { res <<- "resolve"; val <<- x },
     1)
  expect_equal(res, "resolve")
  expect_equal(val, 42)
})

test_that("def__make_parent_resolve", {
  ## NULL
  r1 <- def__make_parent_reject(NULL)
  res <- NULL
  val <- NULL
  expect_error(
    r1("foobar",
       function(x) { res <<- "resolve"; val <<- x },
       1),
    "foobar"
  )
  expect_null(res)
  expect_null(val)

  ## function without args
  r2 <-  def__make_parent_reject(function() 42 * 42)
  res <- NULL
  val <- NULL
  r2(42,
     function(x) { res <<- "resolve"; val <<- x },
     1)
  expect_equal(res, "resolve")
  expect_equal(val, 42 * 42)

  ## function with value arg
  r2 <-  def__make_parent_reject(function(val) val)
  res <- NULL
  val <- NULL
  r2(42,
     function(x) { res <<- "resolve"; val <<- x },
     1)
  expect_equal(res, "resolve")
  expect_equal(val, 42)
})
