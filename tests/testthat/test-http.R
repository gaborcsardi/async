
context("HTTP")

test_that("GET", {

  skip_if_offline()

  do <- async(function() {
    result <- await(http_get("https://eu.httpbin.org/get?q=42")$
                    then(~ rawToChar(.$content)))
    expect_match(result, "\"q\": \"42\"", fixed = TRUE)
  })
  synchronise(do())
})

test_that("HEAD", {

  skip_if_offline()

  do <- async(function() {
    dx <- http_head("https://eu.httpbin.org")$then(function(value) {
      expect_equal(value$status_code, 200)
    })
    await(dx)
  })
  synchronise(do())
})

test_that("headers", {

  skip_if_offline()

  do <- async(function() {
    headers = c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
    dx <- http_get("https://eu.httpbin.org/headers", headers = headers)$
      then(~ jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE))

    expect_equal(await(dx)$headers$`X-Header-Test`, "foobar")
    expect_equal(await(dx)$headers$`X-Another`, "boooyakasha")
  })
  synchronise(do())
})

test_that("304 is not an error", {

  skip_if_offline()

  do <- async(function() {
    dx <- http_get("https://eu.httpbin.org/status/304")$
      then(http_stop_for_status)
    expect_silent(await(dx))
  })
  synchronise(do())
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

    await(dx)

    expect_equal(await(dx)$status_code, 200)
    expect_true(file.exists(tmp))
    expect_equal(file.info(tmp)$size, amountx)
    expect_equal(totalx, amountx)
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

    expect_equal(await(dx)$status_code, 304)
    expect_equal(statusx, 304)
    expect_equal(length(await(dx)$content), 0)
    expect_false(file.exists(tmp))
  })
  synchronise(do())
})

test_that("progress bar for in-memory data", {

  skip_if_offline()

  u1 <- "http://httpbin.org/stream-bytes/2048?chunk_size=1024"

  called <- 0L
  bytes <- 0L
  do <- async(function() {
    dx <- http_get(
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
