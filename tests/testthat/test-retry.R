
context("retry")

test_that("unsuccessful retry", {

  x <- 5
  expect_error(
    await(retry(
      function() {
        x <<- x - 1
        if (x) stop("error") else "OK"
      },
      times = 3
    )),
    "error"
  )
})

test_that("successful retry", {

  x <- 5
  result <- await(retry(
    function() {
      x <<- x - 1
      if (x) stop("error") else "OK"
    },
    times = 5
  ))

  expect_equal(result, "OK")
})
