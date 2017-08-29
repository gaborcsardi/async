
context("await")

test_that("await with multiple tasks", {

  skip_if_offline()

  dx1 <- http_get("https://eu.httpbin.org/get")$then(~ .$status_code)
  dx2 <- http_get("https://eu.httpbin.org/get?q=42")$then(~ .$status_code)

  await_list(dx1, dx2)
  expect_equal(await(dx1), 200)
  expect_equal(await(dx2), 200)
})
