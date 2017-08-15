
context("parallel")

test_that("parallel", {

  skip_if_offline()

  res <- NULL

  await(parallel(
    list(
      function(callback) {
        http_get("https://eu.httpbin.org/get?q=foo",
                 function(err, res) callback(err, rawToChar(res$content)))
      },
      function(callback) {
        http_get("https://eu.httpbin.org/get?q=bar",
                 function(err, res) callback(err, rawToChar(res$content)))
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
    await(parallel_limit(
      list(
        function(callback) callback(NULL, 1),
        function(callback) callback(NULL, 2),
        function(callback) callback(NULL, 3),
        function(callback) callback(NULL, 4),
        function(callback) callback(NULL, 5)
      ),
      limit = limit,
      function(err, res) { error <<- err; result <<- res }
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
