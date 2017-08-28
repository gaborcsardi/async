
context("deferred then")

test_that("HTTP HEAD & synchronous then", {
  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")
  dx2 <- dx$then(function(value) value$status_code)

  await(dx2)
  expect_equal(dx2$get_value(), 200)
})

test_that("HTTP HEAD & async then", {
  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")
  dx2 <- dx$then(function(value) http_get(value$url))
  x <- await(dx2)

  expect_equal(dx2$get_value()$status_code, 200)
})
