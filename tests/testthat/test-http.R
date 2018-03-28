
context("HTTP")

test_that("GET", {

  skip_if_offline()

  do <- async(function() {
    http_get("https://eu.httpbin.org/get?q=42")$
      then(~ rawToChar(.$content))$
      then(~ expect_match(., "\"q\": \"42\"", fixed = TRUE))
  })
  synchronise(do())
})

test_that("HEAD", {

  skip_if_offline()

  do <- async(function() {
    http_head("https://eu.httpbin.org")$
      then(function(value) {
        expect_equal(value$status_code, 200)
      })
  })
  synchronise(do())
})

test_that("headers", {

  skip_if_offline()

  do <- async(function() {
    headers = c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
    dx <- http_get("https://eu.httpbin.org/headers", headers = headers)$
      then(~ jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE))

    when_all(
      dx$then(~ expect_equal(.$headers$`X-Header-Test`, "foobar")),
      dx$then(~ expect_equal(.$headers$`X-Another`, "boooyakasha"))
    )
  })
  synchronise(do())
})

test_that("304 is not an error", {

  skip_if_offline()

  do <- async(function() {
    http_get("https://eu.httpbin.org/status/304")$
      then(http_stop_for_status)
  })
  expect_silent(synchronise(do()))
})

test_that("http progress bars", {

  skip_if_offline()

  do <- async(function() {
    totalx <- NULL
    amountx <- 0
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    dx <- http_get(
      "https://eu.httpbin.org/image/jpeg",
      file = tmp <- tempfile(),
      on_progress = function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$amount)) amountx <<- amountx + data$amount
      }
    )

    when_all(
      dx$then(~ expect_equal(.$status_code, 200)),
      dx$then(~ expect_true(file.exists(tmp))),
      dx$then(~ expect_equal(file.info(tmp)$size, amountx)),
      dx$then(~ expect_equal(totalx, amountx))
    )
  })
  synchronise(do())
})

test_that("http progress bars & etags", {

  skip_if_offline()

  do <- async(function() {
    totalx <- NULL
    amountx <- NULL
    statusx <- NULL
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    dx <- http_get(
      "https://eu.httpbin.org/etag/etag",
      file = tmp <- tempfile(),
      headers = c("If-None-Match" = "etag"),
      on_progress = function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        amountx <<- c(amountx, data$amount)
        statusx <<- data$status_code
      }
    )

    when_all(
      dx$then(~ expect_equal(.$status_code, 304)),
      dx$then(~ expect_equal(statusx, 304)),
      dx$then(~ expect_equal(length(.$content), 0)),
      dx$then(~ expect_false(file.exists(tmp)))
    )
  })
  synchronise(do())
})

test_that("progress bar for in-memory data", {

  skip_if_offline()

  u1 <- "http://httpbin.org/stream-bytes/2048?chunk_size=1024"

  called <- 0L
  bytes <- 0L
  do <- async(function() {
    http_get(
      u1, options = list(buffersize = 1100),
      on_progress = function(data) {
        called <<- called + 1L
        if (length(data$amount)) bytes <<- bytes + data$amount
      }
    )
  })

  ret <- synchronise(do())
  expect_true(called >= 2)
  expect_equal(bytes, 2048)
  expect_equal(length(ret$content), 2048)
})

test_that("error, invalid arg", {

  do <- function() {
    dx <- http_get(12123)
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
})

test_that("automatic cancellation", {

  skip_if_offline()

  called <- 0L
  do <- function() {
    r1 <- http_get("https://httpbin.org/delay/5")$
      then(function() called <<- called + 1L)
    r2 <- http_get("https://httpbin.org/get")$
      then(function() called <<- called + 1L)
    when_any(r1, r2)
  }

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()

  expect_equal(called, 1L)
  expect_true(toc - tic < as.difftime(4, units = "secs"))
})

test_that("http_status",  {
  expect_error(
    http_status(0),
    "Unknown http status code"
  )
})

test_that("timeout, failed request", {

  skip_if_offline()

  do <- function() {
    http_get("https://httpbin.org/delay/5", options = list(timeout = 1))
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "timed out")
  expect_true(toc - tic < as.difftime(4, units = "secs"))

  do2 <- function() {
    do()$catch(~ "fixed")
  }

  tic <- Sys.time()
  res <- synchronise(do2())
  toc <- Sys.time()

  expect_equal(res, "fixed")
  expect_true(toc - tic < as.difftime(4, units = "secs"))
})

test_that("errors contain the response", {

  skip_if_offline()

  do <- function() {
    http_get("https://httpbin.org/status/418")$
      then(http_stop_for_status)
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_s3_class(err, "async_http_418")
  expect_match(rawToChar(err$response$content), "teapot")
})
