
context("http")

test_that("GET", {

  skip_if_offline()

  do <- async(function() {
    http_get("https://eu.httpbin.org/get?q=42")$
      then(~ rawToChar(.$content))$
      then(~ expect_match(., "\"q\":[ ]*\"42\""))
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

  xx <- NULL
  do <- async(function() {
    headers = c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
    http_get("https://eu.httpbin.org/headers", headers = headers)$
      then(~ jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE))$
      then(function(x) xx <<- x)
  })
  synchronise(do())
  expect_equal(xx$headers$`X-Header-Test`, "foobar")
  expect_equal(xx$headers$`X-Another`, "boooyakasha")
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

  xx <- NULL
  totalx <- NULL
  currentx <- 0
  tmp <- tempfile()

  do <- async(function() {
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    http_get(
      "https://eu.httpbin.org/image/jpeg",
      file = tmp <<- tempfile(),
      on_progress = function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        if (!is.null(data$current)) currentx <<- data$current
      }
    )$then(function(x) xx <<- x)
  })

  synchronise(do())

  expect_equal(xx$status_code, 200)
  expect_true(file.exists(tmp))
  expect_equal(file.info(tmp)$size, currentx)
  expect_equal(totalx, currentx)
})

test_that("http progress bar, remove callback", {
  skip_if_offline()

  xx <- NULL
  totalx <- NULL
  currentx <- 0
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  do <- async(function() {
    progress_callback <- function(data) {
      if (!is.null(data$total)) totalx <<- data$total
      if (!is.null(data$current)) currentx <<- data$current
    }
    hx <- http_get(
      "https://eu.httpbin.org/image/jpeg",
      file = tmp <<- tempfile(),
      on_progress = progress_callback)

    rm(progress_callback)
    gc(); gc()

    hx$then(function(x) xx <<- x)
  })

  synchronise(do())

  expect_equal(xx$status_code, 200)
  expect_true(file.exists(tmp))
  expect_equal(file.info(tmp)$size, currentx)
  expect_equal(totalx, currentx)
})

test_that("http progress bars & etags", {

  skip_if_offline()

  xx <- NULL
  totalx <- NULL
  currentx <- NULL
  statusx <- NULL
  tmp <- tempfile()

  do <- async(function() {
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    http_get(
      "https://eu.httpbin.org/etag/etag",
      file = tmp,
      headers = c("If-None-Match" = "etag"),
      on_progress = function(data) {
        if (!is.null(data$total)) totalx <<- data$total
        currentx <<- c(currentx, data$current)
        statusx <<- curl::handle_data(data$handle)$status_code
      }
    )$then(function(x) xx <<- x)
  })
  synchronise(do())
  expect_equal(xx$status_code, 304)
  expect_equal(statusx, 304)
  expect_equal(length(xx[["content"]]), 0)
  expect_true(file.exists(tmp))
  expect_equal(file.info(tmp)$size, 0)
})

test_that("progress bar for in-memory data", {

  skip_if_offline()

  u1 <- "http://eu.httpbin.org/stream-bytes/2048?chunk_size=1024"

  called <- 0L
  bytes <- 0L
  do <- async(function() {
    http_get(
      u1, options = list(buffersize = 1100),
      on_progress = function(data) {
        called <<- called + 1L
        if (length(data$current)) bytes <<- data$current
      }
    )
  })

  ret <- synchronise(do())
  expect_true(called >= 2)
  ## Skip this for now, curl 3.2 seems to be misreporting it
  ## expect_equal(bytes, 2048)
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
    r1 <- http_get("https://eu.httpbin.org/delay/5")$
      then(function() called <<- called + 1L)
    r2 <- http_get("https://eu.httpbin.org/get")$
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
    http_get("https://eu.httpbin.org/delay/5", options = list(timeout = 1))
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "timed out")
  expect_true(toc - tic < as.difftime(4, units = "secs"))

  do2 <- function() {
    do()$catch(error = ~ "fixed")
  }

  tic <- Sys.time()
  res <- synchronise(do2())
  toc <- Sys.time()

  expect_equal(res, "fixed")
  expect_true(toc - tic < as.difftime(4, units = "secs"))
})

test_that("more sophisticated timeouts", {

  skip_if_offline()

  do <- function() {
    withr::local_options(list(
      async_http_timeout = 6,
      async_http_low_speed_time = 2,
      async_http_low_speed_limit = 10
    ))
    http_get("https://eu.httpbin.org/drip?duration=5&numbytes=10&code=200&delay=0")
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "too slow")
  expect_true(toc - tic < as.difftime(5, units = "secs"))
})

test_that("errors contain the response", {

  skip_if_offline()

  do <- function() {
    http_get("https://eu.httpbin.org/status/418")$
      then(http_stop_for_status)
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_s3_class(err, "async_http_418")
  expect_match(rawToChar(err$response$content), "teapot")
})

test_that("errors contain the response if 'file' arg given", {
  skip_if_offline()

  tmp <- tempfile()
  do <- function() {
    http_get("https://eu.httpbin.org/status/418", file = tmp)$
      then(http_stop_for_status)
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_s3_class(err, "async_http_418")
  expect_true(any(grepl("teapot", readLines(tmp))))
})

test_that("http_post", {
  resp <- NULL
  obj <- list(baz = 100, foo = "bar")
  data <- jsonlite::toJSON(obj)

  do <- function() {
    headers <- c("content-type" = "application/json")
    http_post("https://eu.httpbin.org/post", data = data, headers = headers)$
      then(http_stop_for_status)$
      then(function(x) resp <<- x)
  }

  synchronise(do())
  expect_equal(resp$status_code, 200)
  cnt <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(cnt$json, obj)
})
