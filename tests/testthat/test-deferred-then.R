
context("deferred then")

test_that("HTTP HEAD & synchronous then", {
  skip_if_offline()

  result <- await(
    dx <- http_head("https://eu.httpbin.org")$
    then(function(value) value$status_code)
  )

  expect_equal(result, 200)
  expect_equal(dx$get_value(), 200)
  expect_equal(await(dx), 200)
})

test_that("HTTP HEAD & async then", {
  skip_if_offline()

  result <- await(
    dx <- http_head("https://eu.httpbin.org")$
    then(function(value) http_get(value$url))
  )

  expect_equal(result$status_code, 200)
  expect_equal(dx$get_value()$status_code, 200)
  expect_equal(await(dx)$status_code, 200)
})

test_that("HTTP HEAD & async then & sync then", {
  skip_if_offline()

  result <- await(
    dx <- http_head("https://eu.httpbin.org") $
    then(function(value) http_get(value$url)) $
    then(function(value) value$status_code)
  )

  expect_equal(result, 200)
  expect_equal(dx$get_value(), 200)
  expect_equal(await(dx), 200)
})

test_that("then for fulfilled", {
  skip_if_offline()

  await(dx <- http_head("https://eu.httpbin.org/status/404"))
  result <- await(dx$then(function(value) value$status_code))

  expect_equal(result, 404)
})

test_that("multiple then clauses", {
  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org/status/404")
  dx2 <- dx$then(function(value) http_get(value$url))
  dx3 <- dx$then(function(value) value$status_code)
  dx4 <- dx$then(function(value) http_head(value$url))

  result <- await_list(dx2, dx3, dx4)

  expect_equal(result[[1]]$status_code, 404)
  expect_equal(result[[2]], 404)
  expect_equal(result[[3]]$url, dx$get_value()$url)

  expect_equal(dx2$get_value()$status_code, 404)
  expect_equal(dx3$get_value(), 404)
  expect_equal(dx4$get_value()$url, dx$get_value()$url)
})
