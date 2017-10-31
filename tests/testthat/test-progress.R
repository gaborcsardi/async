
context("progress reporting")

test_that("tick", {

  do <- async(function() {
    dx <- deferred$new(
      function(resolve, reject, progress) {
        for (i in 1:10) progress(tick = 1)
        progress(tick = 1)
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
        for (i in 1:10) progress(tick = 1)
        resolve("done")
      },
      function(tick) ticked <<- ticked + tick
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
        progress(total = 10)
        for (i in 1:10) progress(tick = 1)
        resolve("done")
      },
      function(tick, total) {
        if (!is.null(total)) totalx <<- total
        if (!is.null(tick)) ticked <<- ticked + tick
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
        for (i in 1:10) progress(ratio = i / 10)
        resolve("done")
      },
      function(ratio) ratiox <<- c(ratiox, ratio)
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
        progress(total = 100)
        for (i in 1:10) progress(amount = 10)
        resolve("done")
      },
      function(amount, total) {
        if (!is.null(total)) totalx <<- total
        if (!is.null(amount)) amountx <<- amountx + amount
      }
    )
    expect_equal(await(dx), "done")
    expect_equal(totalx, 100)
    expect_equal(amountx, 100)
  })
  synchronise(do())
})
