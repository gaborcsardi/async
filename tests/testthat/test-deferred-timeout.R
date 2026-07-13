test_that("deferred timeout", {
  skip_on_cran()

  good <- FALSE
  tic <- Sys.time()
  do <- async(function() {
    delay(1 / 4)$then(function(value) good <<- TRUE)
  })
  synchronise(do())
  expect_true(Sys.time() - tic >= as.difftime(1 / 4, unit = "secs"))
  expect_true(good)
})
