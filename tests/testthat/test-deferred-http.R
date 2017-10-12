
context("deferred http")

test_that("GET", {

  skip_if_offline()

  dx <- http_get("https://eu.httpbin.org/get?q=42")
  wait_for(dx)

  result <- rawToChar(dx$get_value()$content)
  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})

test_that("HEAD", {

  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")
  wait_for(dx)

  expect_equal(dx$get_value()$status_code, 200)
})

test_that("http_stop_for_status", {
  dx <- http_get("https://httpbin.org/status/404")$
    then(http_stop_for_status)

  expect_error(wait_for(dx), "HTTP error")
})
