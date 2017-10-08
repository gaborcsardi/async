
context("HTTP")

test_that("GET", {

  skip_if_offline()

  result <- await(http_get("https://eu.httpbin.org/get?q=42")$
                  then(~ rawToChar(.$content)))

  expect_match(result, "\"q\": \"42\"", fixed = TRUE)
})

test_that("HEAD", {

  skip_if_offline()

  dx <- http_head("https://eu.httpbin.org")$then(function(value) {
    expect_equal(value$status_code, 200)
  })

  await(dx)
})

test_that("headers", {

  skip_if_offline()

  headers = c("X-Header-Test" = "foobar", "X-Another" = "boooyakasha")
  dx <- http_get("https://eu.httpbin.org/headers", headers = headers)$
    then(~ jsonlite::fromJSON(rawToChar(.$content), simplifyVector = FALSE))

  expect_equal(await(dx)$headers$`X-Header-Test`, "foobar")
  expect_equal(await(dx)$headers$`X-Another`, "boooyakasha")
})

test_that("304 is not an error", {

  skip_if_offline()

  dx <- http_get("https://httpbin.org/status/304")$
    then(http_stop_for_status)
  expect_silent(await(dx))
})

test_that("http progress bars", {

  skip_if_offline()

  totalx <- NULL
  amountx <- integer()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dx <- http_get(
    "https://httpbin.org/image/jpeg",
    file = tmp <- tempfile(),
    on_progress = function(total, amount, status_code) {
      if (!is.null(total)) totalx <<- total
      amountx <<- c(amountx, amount)
    }
  )

  await(dx)

  expect_equal(await(dx)$status_code, 200)
  expect_true(file.exists(tmp))
  expect_equal(file.size(tmp), utils::tail(amountx, 1))
  expect_equal(totalx, utils::tail(amountx, 1))
})

test_that("http progress bars & etags", {

  skip_if_offline()

  totalx <- NULL
  amountx <- NULL
  statusx <- NULL
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dx <- http_get(
    "https://httpbin.org/etag/etag",
    file = tmp <- tempfile(),
    headers = c("If-None-Match" = "etag"),
    on_progress = function(total, amount, status_code) {
      if (!is.null(total)) totalx <<- total
      amountx <<- c(amountx, amount)
      statusx <<- status_code
    }
  )

  expect_equal(await(dx)$status_code, 304)
  expect_equal(statusx, 304)
  expect_equal(length(await(dx)$content), 0)
  expect_false(file.exists(tmp))
})
