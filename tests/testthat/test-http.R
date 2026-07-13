test_that("GET", {
  do <- async(function() {
    http_get(http$url("/get", query = list(q = 42)))$then(function(.) {
      rawToChar(.$content)
    })$then(function(.) expect_match(., "\"q\":[ ]*\"42\""))
  })
  synchronise(do())
})

test_that("HEAD", {
  do <- async(function() {
    http_head(http$url("/"))$then(function(value) {
      expect_equal(value$status_code, 200)
    })
  })
  synchronise(do())
})

test_that("headers", {
  xx <- NULL
  do <- async(function() {
    headers <- c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
    http_get(http$url("/headers"), headers = headers)$then(function(.) {
      jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE)
    })$then(function(x) xx <<- x)
  })
  synchronise(do())
  expect_equal(xx$headers$`X-Header-Test`, "foobar")
  expect_equal(xx$headers$`X-Another`, "boooyakasha")
})

test_that("304 is not an error", {
  do <- async(function() {
    http_get(http$url("/status/304"))$then(http_stop_for_status)
  })
  expect_silent(synchronise(do()))
})

test_that("http progress bars", {
  xx <- NULL
  totalx <- NULL
  currentx <- 0
  tmp <- tempfile()

  do <- async(function() {
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    http_get(
      http$url("/image/jpeg"),
      file = tmp <<- tempfile(),
      on_progress = function(data) {
        if (!is.null(data$total)) {
          totalx <<- data$total
        }
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
  xx <- NULL
  totalx <- NULL
  currentx <- 0
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  do <- async(function() {
    progress_callback <- function(data) {
      if (!is.null(data$total)) {
        totalx <<- data$total
      }
      if (!is.null(data$current)) currentx <<- data$current
    }
    hx <- http_get(
      http$url("/image/jpeg"),
      file = tmp <<- tempfile(),
      on_progress = progress_callback
    )

    rm(progress_callback)
    gc()
    gc()

    hx$then(function(x) xx <<- x)
  })

  synchronise(do())

  expect_equal(xx$status_code, 200)
  expect_true(file.exists(tmp))
  expect_equal(file.info(tmp)$size, currentx)
  expect_equal(totalx, currentx)
})

test_that("http progress bars & etags", {
  xx <- NULL
  totalx <- NULL
  currentx <- NULL
  statusx <- NULL
  tmp <- tempfile()

  do <- async(function() {
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    http_get(
      http$url("/etag/etag"),
      file = tmp,
      headers = c("If-None-Match" = "etag"),
      on_progress = function(data) {
        if (!is.null(data$total)) {
          totalx <<- data$total
        }
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
  u1 <- http$url("/stream-bytes/2048", c(chunk_size = 1024))

  called <- 0L
  bytes <- 0L
  do <- async(function() {
    http_get(
      u1,
      options = list(buffersize = 1100),
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
  called <- 0L
  do <- function() {
    r1 <- http_get(http$url("/delay/5"), options = list(http_version = 2))$then(
      function() called <<- called + 1L
    )
    r2 <- http_get(http$url("/get"), options = list(http_version = 2))$then(
      function() called <<- called + 1L
    )
    when_any(r1, r2)
  }

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()

  expect_equal(called, 1L)
  expect_true(toc - tic < as.difftime(4, units = "secs"))
})

test_that("http_status", {
  expect_error(
    http_status(0),
    "Unknown http status code"
  )
})

test_that("timeout, failed request", {
  do <- function() {
    http_get(http$url("/delay/5"), options = list(timeout = 1), retry = FALSE)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "timed out")
  expect_true(toc - tic < as.difftime(4, units = "secs"))

  do2 <- function() {
    do()$catch(error = function(.) "fixed")
  }

  tic <- Sys.time()
  res <- synchronise(do2())
  toc <- Sys.time()

  expect_equal(res, "fixed")
  expect_true(toc - tic < as.difftime(4, units = "secs"))
})

test_that("more sophisticated timeouts", {
  do <- function() {
    withr::local_options(list(
      async_http_timeout = 6,
      async_http_low_speed_time = 2,
      async_http_low_speed_limit = 10
    ))
    http_get(
      http$url(
        "/drip",
        c(duration = 5, numbytes = 10, code = 200, delay = 0)
      ),
      retry = FALSE
    )
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "too slow")
  expect_true(toc - tic < as.difftime(5, units = "secs"))
})

test_that("http_version is set from options and env vars", {
  # explicit option wins
  expect_equal(
    get_default_curl_options(list(http_version = 3))$http_version,
    3L
  )

  # async_http_http_version option
  withr::local_options(async_http_http_version = 3)
  expect_equal(get_default_curl_options(list())$http_version, 3L)

  # ASYNC_HTTP_HTTP_VERSION env var (option has priority)
  withr::local_options(async_http_http_version = NULL)
  withr::local_envvar(ASYNC_HTTP_HTTP_VERSION = "3")
  expect_equal(get_default_curl_options(list())$http_version, 3L)

  # default is used when nothing is set
  withr::local_envvar(ASYNC_HTTP_HTTP_VERSION = NA_character_)
  expect_equal(
    get_default_curl_options(list())$http_version,
    default_http_version()
  )
})

test_that("default_http_version", {
  expect_equal(default_http_version(), 2L)
})

test_that("errors contain the response", {
  do <- function() {
    http_get(http$url("/status/418"))$then(http_stop_for_status)
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_s3_class(err, "async_http_418")
  expect_match(rawToChar(err$response$content), "teapot")
})

test_that("errors contain the response if 'file' arg given", {
  tmp <- tempfile()
  do <- function() {
    http_get(http$url("/status/418"), file = tmp)$then(http_stop_for_status)
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
    http_post(http$url("/post"), data = data, headers = headers)$then(
      http_stop_for_status
    )$then(function(x) resp <<- x)
  }

  synchronise(do())
  expect_equal(resp$status_code, 200)
  cnt <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(cnt$json, obj)
})

test_that("http_post file", {
  resp <- NULL
  obj <- list(baz = 100, foo = "bar")
  data <- jsonlite::toJSON(obj)
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(data), tmp)

  do <- function() {
    headers <- c("content-type" = "application/json")
    http_post(http$url("/post"), data_file = tmp, headers = headers)$then(
      http_stop_for_status
    )$then(function(x) resp <<- x)
  }

  synchronise(do())
  expect_equal(resp$status_code, 200)
  cnt <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
  expect_equal(cnt$json, obj)
})

test_that("http_post form", {
  local_edition(3)
  resp <- NULL
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw("0123456789"), tmp)

  do <- function() {
    form <- list(
      foo = curl::form_data("bar"),
      baz = curl::form_file(tmp, type = "text/plain", name = "mrfile")
    )
    http_post(http$url("/post"), data_form = form)$then(
      http_stop_for_status
    )$then(function(x) resp <<- x)
  }

  resp <- synchronise(do())
  obj <- jsonlite::fromJSON(rawToChar(resp$content))
  expect_snapshot(obj$files)
  expect_snapshot(obj$form)
})

test_that("get_default_http_retry normalization", {
  expect_null(get_default_http_retry(FALSE, "GET"))
  expect_null(get_default_http_retry(NULL, "GET"))
  expect_null(get_default_http_retry(0, "GET"))

  # POST is not retryable with the defaults
  expect_null(get_default_http_retry(TRUE, "POST"))

  # GET uses the documented defaults
  r <- get_default_http_retry(TRUE, "GET")
  expect_equal(r$limit, 2L)
  expect_true(all(c(408L, 429L, 503L) %in% r$status_codes))
  expect_true(r$errors)

  # a single number sets the limit
  expect_equal(get_default_http_retry(5, "GET")$limit, 5L)

  # list entries override the defaults, methods are case insensitive
  r <- get_default_http_retry(list(limit = 1, methods = "post"), "POST")
  expect_equal(r$limit, 1L)

  # unknown list entries and invalid types are errors
  expect_error(get_default_http_retry(list(bad = 1), "GET"), "Unknown")
  expect_error(get_default_http_retry("nope", "GET"), "Invalid")
})

test_that("http_retry_after parses the header", {
  mkresp <- function(h) list(headers = charToRaw(h))
  expect_equal(
    http_retry_after(mkresp("HTTP/1.1 503\r\nRetry-After: 7\r\n\r\n")),
    7
  )
  expect_null(http_retry_after(mkresp("HTTP/1.1 503\r\n\r\n")))
  expect_null(http_retry_after(list(headers = NULL)))
  # a date in the past means retry immediately
  expect_equal(
    http_retry_after(mkresp(
      "HTTP/1.1 503\r\nRetry-After: Wed, 21 Oct 2015 07:28:00 GMT\r\n\r\n"
    )),
    0
  )
})

test_that("retryable status codes are retried", {
  app <- webfakes::new_app()
  app$locals$n <- 0L
  app$get("/flaky", function(req, res) {
    req$app$locals$n <- req$app$locals$n + 1L
    n <- req$app$locals$n
    if (n < 3L) {
      res$set_status(503L)$send("nope")
    } else {
      res$set_status(200L)$send(as.character(n))
    }
  })
  proc <- webfakes::new_app_process(app)
  on.exit(proc$stop(), add = TRUE)

  # default limit is 2, so the third attempt (a 200) succeeds.
  # `backoff` is set to zero so the test does not sleep between retries.
  resp <- synchronise(http_get(
    proc$url("/flaky"),
    retry = list(backoff = function(i) 0)
  ))
  expect_equal(resp$status_code, 200L)
  expect_equal(rawToChar(resp$content), "3")
})

test_that("retries are exhausted and the last response is returned", {
  resp <- synchronise(http_get(
    http$url("/status/503"),
    retry = list(limit = 1, backoff = function(i) 0)
  ))
  expect_equal(resp$status_code, 503L)
})

test_that("retry = FALSE and non-retryable codes do not retry", {
  # neither request retries, so no backoff happens
  # 404 is not in the default status_codes
  resp <- synchronise(http_get(http$url("/status/404")))
  expect_equal(resp$status_code, 404L)
  # retry = FALSE disables retries entirely
  resp <- synchronise(http_get(http$url("/status/503"), retry = FALSE))
  expect_equal(resp$status_code, 503L)
})

test_that("connection errors are retried unless disabled", {
  # RFC 2606 reserves `.invalid`, a refused port is slow on windows.
  url <- "http://nao.invalid/nope"
  err <- tryCatch(
    synchronise(http_get(
      url,
      retry = list(limit = 1, backoff = function(i) 0)
    )),
    error = identity
  )
  expect_s3_class(err, "async_rejected")

  err2 <- tryCatch(
    synchronise(http_get(
      url,
      retry = list(limit = 5, errors = FALSE)
    )),
    error = identity
  )
  expect_s3_class(err2, "async_rejected")
})

test_that("curl multi options", {
  # It is not possible to query the options that were set on a handle,
  # so this is not a great test case.
  withr::local_options(
    async_http_total_con = 1,
    async_http_host_con = 1,
    async_multiplext = 1
  )
  do <- function() {
    http_get(http$url("/delay/0.34"))
  }
  tic <- proc.time()["elapsed"]
  synchronise(when_all(do(), do(), do()))
  toc <- proc.time()["elapsed"]
  expect_true(toc - tic > as.difftime(1, units = "secs"))
})
