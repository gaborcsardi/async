
test_that("HTTP HEAD & synchronous then", {
  do <- function() {
    http_head(http$url("/"))$
      then(function(value) value$status_code)$
      then(function(x) expect_equal(x, 200))
  }
  synchronise(do())
})

test_that("HTTP HEAD & async then", {
  do <- function() {
    http_head(http$url("/"))$
      then(function(value) http_get(value$url))$
      then(function(value) expect_equal(value$status_code, 200))
  }
  synchronise(do())
})

test_that("HTTP HEAD & async then & sync then", {
  do <- function() {
    http_head(http$url("/"))$
      then(function(value) http_get(value$url))$
      then(function(value) value$status_code)$
      then(function(value) expect_equal(value, 200))
  }
  synchronise(do())
})

test_that("then for fulfilled", {
  do <- async(function() {
    dx <- http_head(http$url("/status/404"))
    dx2 <- http_head(http$url("/status/404"))
    dx$then(function() {
      dx2$
        then(function(value) value$status_code)$
        then(function(value) expect_equal(value, 404))
    })
  })
  synchronise(do())
})

test_that("multiple then clauses are not allowed", {
  do <- async(function() {
    dx <- delay(1/1000)
    dx$then(function() 1)
    dx$then(function() 2)
  })

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "already owned")
})

test_that("compact function notation", {
  do <- function() {
    http_head(http$url("/"))$
      then(function(.) http_get(.$url))$
      then(function(.) .$status_code)$
      then(function(.) expect_equal(., 200))
  }
  synchronise(do())
})

test_that("embedded then", {
  add1 <- function(n) { n ; delay(10/1000)$then(function(value) n + 1) }
  mul3 <- function(n) { n ; delay(10/1000)$then(function(value) n * 3) }

  do <- function() {
    add1(4)$
      then(mul3)$
      then(function(.) expect_equal(., 15))
  }
  synchronise(do())
})

test_that("more embedded thens", {

  steps <- numeric()
  do <- function() {
    async(function() steps <<- c(steps, 1))()$
      then(function() {
        async_constant()$
          then(function() steps <<- c(steps, 2))$
          then(function() steps <<- c(steps, 3))
      })$
      then(function() {
        async_constant()$
          then(function() steps <<- c(steps, 4))$
          then(function() steps <<- c(steps, 5))
      })$
      then(function() steps <<- c(steps, 6))
  }
  synchronise(do())
  expect_equal(steps, 1:6)
})
