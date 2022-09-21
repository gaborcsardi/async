
test_that("async_retryable", {

  do <- async(function() {
    i <- 1
    f <- function() {
      i <<- i + 1
      if (i < 5) stop("error") else "OK"
    }

    async_retryable(f, 5)()
  })
  expect_identical(synchronise(do()), "OK")
})
