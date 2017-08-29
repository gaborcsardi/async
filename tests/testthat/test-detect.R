
context("detect")

test_that("detect", {

  skip("need to rewrite with deferred")
  
  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))

  result <- NULL
  wait_for(detect(1:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  wait_for(detect(2:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(3L, 5L, 7L, 9L))

  result <- "blah"
  wait_for(detect(2, is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- NULL
  wait_for(detect(c(1:10 * 2L, 43L), is_odd,
               function(err, res) result <<- res))
  expect_identical(result, 43L)

  result <- "blah"
  wait_for(detect(numeric(), is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- "blah"
  wait_for(detect(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_null(result)
})

test_that("detect_limit", {

  skip("need to rewrite with deferred")
  
  num <- 0
  task <- function(x, callback) {
    num <<- num + 1
    set_timeout(1/10, function() { num <<- num - 1; callback(NULL, FALSE) })
  }

  result <- "not null"
  wait_for(detect_series(1:5, task, function(err, res) result <<- res))

  expect_null(result)
})
