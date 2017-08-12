
context("parallel")

test_that("parallel", {

  skip_if_offline()

  res <- NULL

  parallel(
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
  )

  await_all()
  expect_equal(length(res), 2)
  expect_false(is.null(res[[1]]))
  expect_false(is.null(res[[2]]))
  expect_match(res[[1]], "\"q\": \"foo\"", fixed = TRUE)
  expect_match(res[[2]], "\"q\": \"bar\"", fixed = TRUE)
})

test_that("empty task list", {

  result <- NULL
  parallel(list(), function(err, res) { result <<- res })
  await_all()
  expect_identical(result, list())
})
