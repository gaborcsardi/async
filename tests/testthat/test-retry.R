
context("async_retry")

test_that("unsuccessful async_retry", {

  do <- async(function() {
    x <- 5
    async_retry(
      function() { x <<- x - 1; if (x) stop("error") else "OK" },
      times = 3
    )$
      catch(error = function(e) expect_match(e$message, "error"))
  })
  synchronise(do())
})

test_that("successful async_retry", {

  do <- async(function() {
    x <- 5
    async_retry(
      function() { x <<- x - 1; if (x) stop("error") else "OK" },
      times = 5
    )
  })
  expect_equal(synchronise(do()), "OK")
})
