
context("async")

test_that("async works", {

  skip_if_offline()

  result <- NULL
  ax <- http_get(
    "https://eu.httpbin.org/get?q=42",
    function(err, res) {
      result <<- rawToChar(res$content)
    }
  )
  await(ax)

  expect_false(is.null(result))
  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})
