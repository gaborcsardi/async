
context("HTTP")

test_that("GET", {

  skip_if_offline()

  result <- NULL
  ax <- http_get(
    "https://eu.httpbin.org/get?q=42",
    function(err, res) {
      result <<- rawToChar(res$content)
    }
  )
  wait_for(ax)

  expect_false(is.null(result))
  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})

test_that("HEAD", {

  skip_if_offline()

  ax <- http_head(
    "https://eu.httpbin.org",
    function(err, res) {
      expect_null(err)
      expect_equal(res$status_code, 200)
    }
  )
  wait_for(ax)
})
