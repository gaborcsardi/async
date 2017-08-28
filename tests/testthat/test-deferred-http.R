
context("deferred http")

test_that("GET", {

  skip_if_offline()

  dx <- http_get("https://eu.httpbin.org/get?q=42")
  await(dx)

  result <- rawToChar(dx$get_value()$content)
  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})

test_that("HEAD", {

  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")
  await(dx)

  expect_equal(dx$get_value()$status_code, 200)
})
