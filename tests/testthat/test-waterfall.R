
context("waterfall")

test_that("waterfall", {

  result <- NULL

  waterfall(
    list(
      function(cb) cb(NULL, "one", "two"),
      function(cb, arg1, arg2) cb(NULL, c(arg1, arg2, "three")),
      function(cb, arg1) cb(NULL, c(arg1, "done"))
    ),
    function(err, res) {
      result <<- res
    }
  )

  await_all()
  expect_identical(result, c("one", "two", "three", "done"))
})
