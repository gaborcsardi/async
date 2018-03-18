
context("deferred then")

test_that("HTTP HEAD & synchronous then", {
  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org")$
      then(function(value) value$status_code)$
      then(function(x) expect_equal(x, 200))
  })
  synchronise(do())
})

test_that("HTTP HEAD & async then", {
  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org")$
      then(function(value) http_get(value$url))$
      then(function(value) expect_equal(value$status_code, 200))
  })
  synchronise(do())
})

test_that("HTTP HEAD & async then & sync then", {
  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org") $
      then(function(value) http_get(value$url))$
      then(function(value) value$status_code)$
      then(function(value) expect_equal(value, 200))
  })
  synchronise(do())
})

test_that("then for fulfilled", {
  skip_if_offline()

  do <- async(function() {
    dx <- http_head("https://eu.httpbin.org/status/404")
    dx$then(function() {
      dx$
        then(function(value) value$status_code)$
        then(function(value) expect_equal(value, 404))
    })
  })
  synchronise(do())
})

test_that("multiple then clauses", {
  skip_if_offline()

  do <- async(function() {
    dx <- http_head("https://eu.httpbin.org/status/404")
    dx2 <- dx$then(function(value) http_get(value$url))
    dx3 <- dx$then(function(value) value$status_code)
    dx4 <- dx$then(function(value) http_head(value$url))

    dxx <- when_all(dx, dx2, dx3, dx4)

    dxx$then(function(result) expect_equal(result[[1]]$status_code, 404))
    dxx$then(function(result) expect_equal(result[[2]], 404))
    dxx$then(function(result) expect_equal(result[[3]]$url, result[[1]]$url))

    dx2$then(function(x) expect_equal(x$status_code, 404))
    dx3$then(function(x) expect_equal(x, 404))
    dx4$then(function(x) {
      dx$then(function(x2) expect_equal(x$url, x2$url))
    })
  })
  synchronise(do())
})

test_that("compact function notation", {
  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org") $
      then(~ http_get(.$url)) $
      then(~ .$status_code)$
      then(~ expect_equal(., 200))
  })
  synchronise(do())
})

test_that("embedded then", {
  add1 <- function(n) { n ; delay(10/1000)$then(function(value) n + 1) }
  mul3 <- function(n) { n ; delay(10/1000)$then(function(value) n * 3) }

  do <- async(function() {
    add1(4)$
      then(mul3)$
      then(~ expect_equal(., 15))
  })
  synchronise(do())
})

test_that("more embedded thens", {

  steps <- numeric()
  do <- async(function() {
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
  })
  synchronise(do())
  expect_equal(steps, 1:6)
})
