
context("waterfall")

test_that("waterfall", {

  result <- NULL

  wait_for(waterfall(
    list(
      function(callback) callback(NULL, "one", "two"),
      function(arg1, arg2, callback) callback(NULL, c(arg1, arg2, "three")),
      function(arg1, callback) callback(NULL, c(arg1, "done"))
    ),
    function(err, res) {
      result <<- res
    }
  ))

  expect_identical(result, c("one", "two", "three", "done"))
})

test_that("waterfall, asyncify", {

  result <- NULL

  wait_for(waterfall(
    list(
      asyncify(function() c("one", "two")),
      asyncify(function(arg12) c(arg12, "three")),
      asyncify(function(arg1) c(arg1, "done"))
    ),
    function(err, res) result <<- res
  ))

  expect_identical(result, c("one", "two", "three", "done"))
})
