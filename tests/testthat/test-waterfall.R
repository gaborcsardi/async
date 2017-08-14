
context("waterfall")

test_that("waterfall", {

  result <- NULL

  await(waterfall(
    list(
      function(callback) callback(NULL, "one", "two"),
      function(callback, arg1, arg2) callback(NULL, c(arg1, arg2, "three")),
      function(callback, arg1) callback(NULL, c(arg1, "done"))
    ),
    function(err, res) {
      result <<- res
    }
  ))

  expect_identical(result, c("one", "two", "three", "done"))
})
