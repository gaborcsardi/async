
context("cancellable")

test_that("auto-cancellation", {

  skip_if_offline()

  http <- NULL
  idx <- 0

  do <- async(function() {
    req_done <- 0L
    
    response_time <- async(function(url) {
      idx <<- idx + 1
      http[[idx]] <<- http_head(url)
      http[[idx]]$
        then(function(x) { req_done <<- req_done + 1L ; x })$
        then(http_stop_for_status)$
        then(~ setNames(.[["times"]][["total"]], url))$
        catch(~ setNames(Inf, url))
    })
    
    urls <- c("https://httpbin.org/delay/5",
              "https://httpbin.org/get")

    reqs <- lapply(urls, response_time)
    when_any(.list = reqs)
  })

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(2, units = "secs"))
  expect_true(get_private(http[[1]])$cancelled)
})
