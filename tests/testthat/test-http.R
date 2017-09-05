
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

test_that("headers", {

  skip_if_offline()

  headers = c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
  dx <- http_get("https://eu.httpbin.org/headers", headers = headers)$
    then(~ jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE))

  expect_equal(await(dx)$headers$`X-Header-Test`, "foobar")
  expect_equal(await(dx)$headers$`X-Another`, "boooyakasha")
})
