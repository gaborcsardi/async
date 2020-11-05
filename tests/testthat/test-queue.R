
test_that("async_queue", {

  do <- function() {
    q <- async_queue(2, 1)
    p1 <- q$async_push(async_constant(0)$then(function() 1))
    p2 <- q$async_push(async_constant(0)$then(function() 2))
    p3 <- q$async_pop()$then(function(x) print(paste("first:", x)))
    p4 <- q$async_pop()$then(function(x) print(paste("second:", x)))
    when_all(p1, p2, p3, p4, q$when_done())
  }

  synchronise(do())

})

test_that("async_queue", {

  do <- function() {
    q <- async_queue(2, 1)
    p1 <- q$async_push(async_constant(0)$then(function() 1))
    p2 <- q$async_push(async_constant(0)$then(function() 2))
    p3 <- q$async_pop()$then(function(x) print(paste("first:", x)))
    p4 <- q$async_pop()$then(function(x) print(paste("second:", x)))
    when_all(q$when_done(), p1, p2, p3, p4)
  }

  synchronise(do())

})

test_that("empty queue", {
  skip("fails")
  do <- function() {
    q <- async_queue(2, 1)
    when_all(q$when_done())
  }
  synchronise(do())
})
