
context("deferred http")

test_that("GET", {
  do <- async(function() {
    http_get(http$url("/get", query = list(q = 42)))$
      then(function(x) rawToChar(x$content))$
      then(function(x) expect_match(x, "\"q\":[ ]*\"42\"", fixed = FALSE))
  })
  synchronise(do())
})

test_that("HEAD", {
  do <- async(function() {
    http_head(http$url("/"))$
      then(function(x) expect_equal(x$status_code, 200))
  })
  synchronise(do())
})

test_that("http_stop_for_status", {
  do <- async(function() {
    http_get(http$url("/status/404"))$
      then(http_stop_for_status)
  })
  expect_error(synchronise(do()), "404", class = "async_http_404")
})
