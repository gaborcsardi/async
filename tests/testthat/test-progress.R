
context("progress reporting")

test_that("tick", {

  do <- async(function() {
    dx <- deferred$new(
      function(resolve, reject, progress) {
        for (i in 1:10) progress(list(tick = 1))
        progress(list(tick = 1))
        resolve("done")
      }
    )

    expect_silent(await(dx))
    expect_equal(await(dx), "done")
  })
  synchronise(do())

  do <- async(function() {
    ticked <- 0
    dx <- deferred$new(
      function(resolve, reject, progress) {
        for (i in 1:10) progress(list(tick = 1))
        resolve("done")
      },
      function(data) ticked <<- ticked + data$tick
    )
    expect_equal(await(dx), "done")
    expect_equal(ticked, 10)
  })
  synchronise(do())
})

test_that("total", {
  do <- async(function() {
    ticked <- 0
    totalx <- NULL
    dx <- deferred$new(
      function(resolve, reject, progress) {
        progress(list(total = 10))
        for (i in 1:10) progress(list(tick = 1))
        resolve("done")
      },
      function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$tick)) ticked <<- ticked + data$tick
      }
    )
    expect_equal(await(dx), "done")
    expect_equal(ticked, 10)
  })
  synchronise(do())
})

test_that("ratio", {
  do <- async(function() {
    ratiox <- 0
    dx <- deferred$new(
      function(resolve, reject, progress) {
        for (i in 1:10) progress(list(ratio = i / 10))
        resolve("done")
      },
      function(data) ratiox <<- c(ratiox, data$ratio)
    )
    expect_equal(await(dx), "done")
    expect_equal(ratiox, (0:10) / 10)
  })
  synchronise(do())
})

test_that("amount", {
  do <- async(function() {
    amountx <- 0
    totalx  <- 0
    dx <- deferred$new(
      function(resolve, reject, progress) {
        progress(list(total = 100))
        for (i in 1:10) progress(list(amount = 10))
        resolve("done")
      },
      function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$amount)) amountx <<- amountx + data$amount
      }
    )
    expect_equal(await(dx), "done")
    expect_equal(totalx, 100)
    expect_equal(amountx, 100)
  })
  synchronise(do())
})
