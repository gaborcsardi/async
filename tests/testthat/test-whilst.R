
context("whilst")

test_that("whilst", {

  count <- 0
  nn <- NULL

  whilst(
    function() count < 5,
    function(callback) {
      count <<- count + 1
      callback(NULL, count)
    },
    function (err, n) {
      nn <<- n
    }
  )

  await_all()
  expect_false(is.null(nn))
  expect_equal(nn, 5)
})

test_that("whilst with false test", {

  result <- NULL

  expect_silent({
    whilst(
      function() FALSE,
      function(callback) {
        stop("Not reached")
        callback(NULL, "Not here")
      },
      function(err, res) {
        result <<- res
      }
    )

    await_all()
  })

  expect_null(result)
})
