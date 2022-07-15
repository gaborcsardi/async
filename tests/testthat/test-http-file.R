
context("http file:://")

test_that("GET file://", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar", file = tmp)

  url <- paste0("file://", normalizePath(tmp))
  ret <- synchronise(http_get(url)$then(http_stop_for_status))
  expect_equal(ret$status_code, 0)
  expect_equal(ret$content, charToRaw("foobar"))
})

test_that("HEAD file://", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar\n", file = tmp)

  url <- paste0("file://", normalizePath(tmp))
  ret <- synchronise(http_head(url)$then(http_stop_for_status))
  expect_equal(ret$status_code, 0)
})

test_that("file:// to file", {
  tmp <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp, tmp2)), add = TRUE)
  cat("foobar", file = tmp)

  url <- paste0("file://", normalizePath(tmp))
  ret <- synchronise(http_get(url, file = tmp2)$then(http_stop_for_status))
  expect_equal(ret$status_code, 0)
  expect_equal(readBin(tmp2, "raw", 100), charToRaw("foobar"))
})

test_that("file:// does not exist", {
  tmp <- tempfile()
  url <- paste0("file://", tmp)
  expect_error(synchronise(http_get(url)))
})
