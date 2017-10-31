
context("deferred then")

test_that("HTTP HEAD & synchronous then", {
  skip_if_offline()

  do <- async(function() {
    result <- await(
      dx <- http_head("https://eu.httpbin.org")$
        then(function(value) value$status_code)
    )

    expect_equal(result, 200)
    expect_equal(dx$get_value(), 200)
    expect_equal(await(dx), 200)
  })
  synchronise(do())
})

test_that("HTTP HEAD & async then", {
  skip_if_offline()

  do <- async(function() {
    result <- await(
      dx <- http_head("https://eu.httpbin.org")$
        then(function(value) http_get(value$url))
    )

    expect_equal(result$status_code, 200)
    expect_equal(dx$get_value()$status_code, 200)
    expect_equal(await(dx)$status_code, 200)
  })
  synchronise(do())
})

test_that("HTTP HEAD & async then & sync then", {
  skip_if_offline()

  do <- async(function() {
    result <- await(
      dx <- http_head("https://eu.httpbin.org") $
        then(function(value) http_get(value$url)) $
        then(function(value) value$status_code)
    )

    expect_equal(result, 200)
    expect_equal(dx$get_value(), 200)
    expect_equal(await(dx), 200)
  })
  synchronise(do())
})

test_that("then for fulfilled", {
  skip_if_offline()

  do <- async(function() {
    await(dx <- http_head("https://eu.httpbin.org/status/404"))
    result <- await(dx$then(function(value) value$status_code))

    expect_equal(result, 404)
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

    result <- await_all(dx2, dx3, dx4)

    expect_equal(result[[1]]$status_code, 404)
    expect_equal(result[[2]], 404)
    expect_equal(result[[3]]$url, dx$get_value()$url)

    expect_equal(dx2$get_value()$status_code, 404)
    expect_equal(dx3$get_value(), 404)
    expect_equal(dx4$get_value()$url, dx$get_value()$url)
  })
  synchronise(do())
})

test_that("compact function notation", {
  skip_if_offline()

  do <- async(function() {
    result <- await(
      dx <- http_head("https://eu.httpbin.org") $
        then(~ http_get(.$url)) $
        then(~ .$status_code)
    )

    expect_equal(result, 200)
    expect_equal(dx$get_value(), 200)
    expect_equal(await(dx), 200)
  })
  synchronise(do())
})

test_that("embedded then", {
  add1 <- function(n) { n ; delay(10/1000)$then(function(value) n + 1) }
  mul3 <- function(n) { n ; delay(10/1000)$then(function(value) n * 3) }

  do <- async(function() {
    result <- await(add1(4)$then(mul3))
    expect_equal(result, 15)
  })
  synchronise(do())
})

test_that("more embedded thens", {

  do <- async(function() {
    steps <- numeric()
    dx <- async(function() steps <<- c(steps, 1))()$
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

    await(dx)
    expect_equal(steps, 1:6)
  })
  synchronise(do())
})
