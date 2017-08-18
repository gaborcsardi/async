
context("detect")

test_that("detect", {

  is_odd <- function(x, callback) callback(NULL, as.logical(x %% 2))

  result <- NULL
  await(detect(1:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))

  result <- NULL
  await(detect(2:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(3L, 5L, 7L, 9L))

  result <- "blah"
  await(detect(2, is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- NULL
  await(detect(c(1:10 * 2L, 43L), is_odd,
               function(err, res) result <<- res))
  expect_identical(result, 43L)

  result <- "blah"
  await(detect(numeric(), is_odd, function(err, res) result <<- res))
  expect_null(result)

  result <- "blah"
  await(detect(1:10 * 2, is_odd, function(err, res) result <<- res))
  expect_null(result)
})

test_that("detect_limit", {

  num <- 0
  task <- function(x, callback) {
    expect_equal(num, 0)
    num <<- num + 1
    set_timeout(1/10, function() { num <<- num - 1; callback(NULL, FALSE) })
  }

  result <- "not null"
  await(detect_series(1:5, task, function(err, res) result <<- res))

  expect_null(result)
})

test_that("with asyncify", {

  is_odd <- asyncify(function(x) as.logical(x %% 2))

  result <- NULL
  await(detect(1:10, is_odd, function(err, res) result <<- res))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))
})

test_that("detect_limit & asyncify", {

  is_odd <- asyncify(function(x) as.logical(x %% 2))

  result <- NULL
  await(detect_limit(1:10, is_odd, 2, function(err, res) result <<- res))
  expect_true(result %in% c(1L, 3L, 5L, 7L, 9L))
})
