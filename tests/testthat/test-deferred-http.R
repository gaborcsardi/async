
context("deferred http")

test_that("GET", {

  skip_if_offline()

  do <- async(function() {
    http_get("https://eu.httpbin.org/get?q=42")$
      then(function(x) rawToChar(x$content))$
      then(function(x) expect_match(x, "\"q\":[ ]*\"42\"", fixed = FALSE))
  })
  synchronise(do())
})

test_that("HEAD", {

  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org")$
      then(function(x) expect_equal(x$status_code, 200))
  })
  synchronise(do())
})

test_that("http_stop_for_status", {

  skip_if_offline()

  do <- async(function() {
    http_get("https://eu.httpbin.org/status/404")$
      then(http_stop_for_status)
  })
  expect_error(synchronise(do()), "404")
})
