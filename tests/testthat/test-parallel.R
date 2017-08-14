
context("parallel")

test_that("parallel", {

  skip_if_offline()

  res <- NULL

  await(parallel(
    list(
      function(cb) {
        http_get("https://eu.httpbin.org/get?q=foo",
                 function(err, res) cb(err, rawToChar(res$content)))
      },
      function(cb) {
        http_get("https://eu.httpbin.org/get?q=bar",
                 function(err, res) cb(err, rawToChar(res$content)))
      }
    ),
    function(err, result) { res <<- result }
  ))

  expect_equal(length(res), 2)
  expect_false(is.null(res[[1]]))
  expect_false(is.null(res[[2]]))
  expect_match(res[[1]], "\"q\": \"foo\"", fixed = TRUE)
  expect_match(res[[2]], "\"q\": \"bar\"", fixed = TRUE)
})

test_that("empty task list", {

  result <- NULL
  await(parallel(list(), function(err, res) { result <<- res }))
  expect_identical(result, list())
})

test_that("limit", {

  test_limit <- function(limit) {
    error <- NULL
    result <- NULL
    await(parallel(
      list(
        function(cb) cb(NULL, 1),
        function(cb) cb(NULL, 2),
        function(cb) cb(NULL, 3),
        function(cb) cb(NULL, 4),
        function(cb) cb(NULL, 5)
      ),
      function(err, res) { error <<- err; result <<- res },
      limit = limit
    ))
    expect_null(error)
    expect_equal(result, as.list(1:5))
  }

  test_limit(1)
  test_limit(2)
  test_limit(4)
  test_limit(5)
  test_limit(10)
})
