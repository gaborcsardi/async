
context("pure_async")

test_that("pure deferred values are marked as expected", {

  p1 <- p2 <- p3 <- NULL
  p <- pure_async(function() {
    p1 <<- delay(1/1000)
    p2 <<- p1$then(delay(1/1000))
    p3 <<- p2$then(delay(1/1000))
    p3
  })

  synchronise(p())

  expect_true(get_private(p1)$locked)
  expect_true(get_private(p2)$locked)
  expect_false(get_private(p3)$locked)
})

test_that("pure_async allows auto-cancellation", {

  skip_if_offline()

  was_cancelled <- NULL

  do <- async(function() {
    req_done <- 0L

    response_time <- pure_async(function(url) {
      http_head(url)$
        then(function(x) { req_done <<- req_done + 1L ; x })$
        then(http_stop_for_status)$
        then(~ setNames(.[["times"]][["total"]], url))$
        catch(~ setNames(Inf, url))
    })

    urls <- c("https://httpbin.org/delay/5",
              "https://httpbin.org/get")

    reqs <- lapply(urls, response_time)
    when_any(.list = reqs)$
      then(~ sort(unlist(.)))$
      then(function() was_cancelled <<- get_private(reqs[[1]])$cancelled)
  })

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(2, units = "secs"))
  expect_true(was_cancelled)
})
