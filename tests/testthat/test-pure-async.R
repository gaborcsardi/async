
context("pure_async")

test_that("pure deferred values are marked as expected", {

  p1 <- p2 <- p3 <- NULL
  p <- pure_async(function() {
    p1 <<- delay(1/1000)
    p2 <<- p1$then(delay(1/1000))
    p3 <<- p2$then(delay(1/1000))
    p3
  })

  synchronise({
    pp <- p()
    expect_false(get_private(pp)$locked)
    pp
  })

  expect_true(get_private(p1)$locked)
  expect_true(get_private(p2)$locked)
  expect_true(get_private(p3)$locked)
})

test_that("non-pure inside pure async", {

  xx <- NULL
  f2 <- async(function() {
    xx <<- delay(1/1000)
    delay(1/1000)
  })

  p1 <- p2 <- p3 <- NULL
  f <- pure_async(function() {
    p1 <<- f2()
    p2 <<- p1$then(delay(1/1000))
    p3 <<- p2$then(delay(1/1000))
    p3$then(~ xx$then(~ "foobar"))
  })

  synchronise(f())

  expect_true(get_private(p1)$locked)
  expect_true(get_private(p2)$locked)
  expect_true(get_private(p3)$locked)
  expect_false(get_private(xx)$locked)
})

test_that("cannot chain on locked promise", {

  p1 <- p2 <- p3 <- NULL
  p <- pure_async(function() {
    p1 <<- delay(1/1000)
    p2 <<- p1$then(delay(1/1000))
    p3 <<- p2$then(delay(1/1000))
    p3
  })

  do <- async(function() {
    px <- p()
    px$then(~ "yes")
    px$then(function() p1$then(~ "no!"))
  })

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "Deferred is locked")
})

test_that("more auto-cancellation", {

  x1 <- x2 <- x3 <- NULL
  f <- pure_async(function() {
    x1 <<- delay(1)
    x2 <<- x1$then(~ 42)
    x3 <<- x2$then(~ . * 42)

    y <- delay(1/100)$then(~ x3$cancel())

    when_all(x3, y)
  })

  err  <- tryCatch(synchronise(f()),  error = identity)
  expect_s3_class(err, "async_cancelled")
  expect_true(get_private(x1)$cancelled)
  expect_true(get_private(x2)$cancelled)
  expect_true(get_private(x3)$cancelled)
})

test_that("when_any auto-cancellation", {

  skip_if_offline()

  http <- NULL
  idx <- 0

  do <- async(function() {

    response_time <- pure_async(function(url) {
      idx <<- idx + 1L
      http[[idx]] <<- http_head(url)
      http[[idx]]$
        then(http_stop_for_status)$
        then(~ setNames(.[["times"]][["total"]], url))$
        catch(~ setNames(Inf, url))
    })

    urls <- c("https://httpbin.org/delay/5",
              "https://httpbin.org/get")

    reqs <- lapply(urls, response_time)
    when_any(.list = reqs, .cancel = TRUE)$
      then(~ sort(unlist(.)))
  })

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(2, units = "secs"))
  expect_true(get_private(http[[1]])$cancelled)
})
