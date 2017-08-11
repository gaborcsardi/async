
context("cancel")

test_that("cancel", {

  skip_if_offline()

  err <- NULL
  ax <- http_get(
    "https://eu.httpbin.org/delay/3",
    function(err, res) { err <<- err }
  )

  cancel(ax)
  expect_false(is.null(err))
})
