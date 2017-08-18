
context("asyncify")

test_that("asyncify", {

  f <- asyncify(function() 42)
  g <- asyncify(function() 42 * 42)

  result <- NULL
  wait_for(parallel(
    list(f, g),
    function(err, res) result <<- res
  ))

  expect_identical(result, list(42, 42 * 42))
})
