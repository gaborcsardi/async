
context("await")

test_that("await with multiple tasks", {

  skip_if_offline()

  status1 <- status2 <- NULL
  ax1 <- http_get(
    "https://eu.httpbin.org/get",
    function(err, res) { status1 <<- res$status_code }
  )
  ax2 <- http_get(
    "https://eu.httpbin.org/get?q=42",
    function(err, res) { status2 <<- res$status_code }
  )

  await(c(ax1, ax2))
  expect_equal(status1, 200)
  expect_equal(status2, 200)
})

test_that("await all", {

  skip_if_offline()

  status1 <- status2 <- NULL
  ax1 <- http_get(
    "https://eu.httpbin.org/get",
    function(err, res) { status1 <<- res$status_code }
  )
  ax2 <- http_get(
    "https://eu.httpbin.org/get?q=42",
    function(err, res) { status2 <<- res$status_code }
  )

  await()
  expect_equal(status1, 200)
  expect_equal(status2, 200)
})
