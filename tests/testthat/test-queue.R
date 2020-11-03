
test_that("push one, pop one", {

  do <- function() {
    q <- make_async_queue(2)
    q$push(delay(1))
    q$push(delay(1))
    p1 <- q$pop()$then(function() print("first"))
    p2 <- q$pop()$then(function() print("second"))
    q$done()
    when_all(p1, p2, q$when_done())
  }

  synchronise(do())
})
