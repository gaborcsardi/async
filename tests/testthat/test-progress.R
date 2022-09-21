
test_that("tick", {

  do <- async(function() {
    deferred$new(
      function(resolve, progress) {
        for (i in 1:10) progress(list(tick = 1))
        progress(list(tick = 1))
        resolve("done")
      }
    )
  })
  expect_equal(synchronise(do()), "done")

  ticked <- 0
  do <- async(function() {
    deferred$new(
      function(resolve, progress) {
        for (i in 1:10) progress(list(tick = 1))
        resolve("done")
      },
      function(data) ticked <<- ticked + data$tick
    )
  })
  expect_equal(synchronise(do()), "done")
  expect_equal(ticked, 10)
})

test_that("total", {
  ticked <- 0
  totalx <- NULL
  do <- async(function() {
    deferred$new(
      function(resolve, progress) {
        progress(list(total = 10))
        for (i in 1:10) progress(list(tick = 1))
        resolve("done")
      },
      function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$tick)) ticked <<- ticked + data$tick
      }
    )
  })
  expect_equal(synchronise(do()), "done")
  expect_equal(ticked, 10)
  expect_equal(totalx, 10)
})

test_that("ratio", {
  ratiox <- 0
  do <- async(function() {
    deferred$new(
      function(resolve, progress) {
        for (i in 1:10) progress(list(ratio = i / 10))
        resolve("done")
      },
      function(data) ratiox <<- c(ratiox, data$ratio)
    )
  })
  expect_equal(synchronise(do()), "done")
  expect_equal(ratiox, (0:10) / 10)
})

test_that("amount", {
  amountx <- 0
  totalx  <- 0
  do <- async(function() {
    deferred$new(
      function(resolve, progress) {
        progress(list(total = 100))
        for (i in 1:10) progress(list(amount = 10))
        resolve("done")
      },
      function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$amount)) amountx <<- amountx + data$amount
      }
    )
  })
  expect_equal(synchronise(do()), "done")
  expect_equal(totalx, 100)
  expect_equal(amountx, 100)
})
