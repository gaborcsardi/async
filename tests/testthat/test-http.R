
context("HTTP")

test_that("GET", {

  skip_if_offline()

  result <- await(http_get("https://eu.httpbin.org/get?q=42")$
                  then(~ rawToChar(.$content)))

  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})

test_that("HEAD", {

  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")$then(function(value) {
    expect_equal(value$status_code, 200)
  })

  await(dx)
})
