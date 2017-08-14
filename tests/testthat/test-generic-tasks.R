
context("generic tasks")

test_that("await on a generic task", {

  skip_if_offline()

  res <- NULL
  p1 <- parallel(
    list(
      function(cb) {
        http_get("https://eu.httpbin.org/delay/1",
                 function(err, res) cb(err, rawToChar(res$content)))
      },
      function(cb) {
        http_get("https://eu.httpbin.org/delay/1",
                 function(err, res) cb(err, rawToChar(res$content)))
      }
    ),
    function(err, result) { res <<- result }
  )

  res2 <- NULL
  tic <- Sys.time()
  p2 <- parallel(
    list(
      function(cb) cb(NULL, "I am done")
    ),
    function(err, result) res2 <<- result
  )

  await(p2)
  expect_true(Sys.time() - tic < as.difftime(1, units = "secs"))
  expect_equal(res2, list("I am done"))

  await(p1)
  expect_false(is.null(res))
  expect_true(Sys.time() - tic > as.difftime(1, units = "secs"))
})
