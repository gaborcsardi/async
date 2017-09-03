
context("whilst")

test_that("whilst", {

  count <- 0
  result <- NULL

  result <- await(whilst(
    function(...) count < 5,
    function() {
      delay(1/1000)$then(function(value) count <<- count + 1)
    }
  ))

  expect_equal(result, 5)
})

test_that("whilst with false test", {

  result <- NULL

  expect_silent({
    await(whilst(
      function() FALSE,
      function() {
        delay(1/1000)$then(function(value) stop("Not reached"))
      }
    ))
  })

  expect_null(result)
})

test_that("error", {

  i <- 1
  expect_error(
    await(whilst(
      function() i < 5,
      function() delay(1/1000)$then(function(value) {
        i <<- i + 1
        if (i >= 3) stop("This is bad")
      })
    )),
    "This is bad"
  )
})
