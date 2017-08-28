
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
