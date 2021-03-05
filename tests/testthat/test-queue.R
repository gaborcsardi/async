
secs <- function(x) as.difftime(x, units = "secs")

test_that("async_queue", {
  do <- function() {
    taskgen <- local({
      cnt <- 0L
      function() {
        cnt <<- cnt + 1L
        if (cnt <= 2) {
          mycnt <- cnt
          delay(0.5)$then(function() mycnt)
        } else {
          quote(exhausted)
        }
      }
    })

    q <- async_queue(taskgen, limit = 1)
    p1 <- q$async_pop()$then(function(x) paste("first:", x))
    p2 <- q$async_pop()$then(function(x) paste("second:", x))
    when_all(q$when_done(), p1, p2)
  }

  expect_true(system.time(ret <- synchronise(do()))[3] >= secs(1))
  expect_equal(ret[[2]], "first: 1")
  expect_equal(ret[[3]], "second: 2")
})

test_that("async_queue", {
  do <- function() {
    taskgen <- local({
      cnt <- 0L
      function() {
        cnt <<- cnt + 1L
        if (cnt <= 2) {
          mycnt <- cnt
          delay(0.5)$then(function() mycnt)
        } else {
          quote(exhausted)
        }
      }
    })

    q <- async_queue(taskgen, limit = 2)
    p1 <- q$async_pop()$then(function(x) paste("first:", x))
    p2 <- q$async_pop()$then(function(x) paste("second:", x))
    when_all(p1, p2, q$when_done())
  }

  expect_true(system.time(ret <- synchronise(do()))[3] < secs(1))
  expect_equal(ret[[1]], "first: 1")
  expect_equal(ret[[2]], "second: 2")
})

test_that("empty queue", {
  do <- function() {
    taskgen <- local({
      function() {
        quote(exhausted)
      }
    })

    q <- async_queue(taskgen, limit = 2)
    p1 <- q$async_pop()$then(function(x) print(paste("first:", x)))
    p2 <- q$async_pop()$then(function(x) print(paste("second:", x)))
    when_all(q$when_done(), p1, p2)
  }

  expect_output(system.time(synchronise(do())), "second: exhausted")
})
