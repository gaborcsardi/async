
context("parallel")

test_that("parallel", {

  skip_if_offline()

  dx1 <- http_get("https://eu.httpbin.org/get?q=foo")$
    then( ~ rawToChar(.$content))
  dx2 <- http_get("https://eu.httpbin.org/get?q=bar")$
    then( ~ rawToChar(.$content))

  await(dx1)
  await(dx2)

  expect_match(dx1$get_value(), "\"q\": \"foo\"", fixed = TRUE)
  expect_match(dx2$get_value(), "\"q\": \"bar\"", fixed = TRUE)
})

test_that("limit", {

  skip("need to rewrite with deferred")  

  test_limit <- function(limit) {
    error <- NULL
    result <- NULL
    wait_for(parallel_limit(
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
