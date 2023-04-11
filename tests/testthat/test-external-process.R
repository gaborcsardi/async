
test_that("external_process", {
  px <- asNamespace("processx")$get_tool("px")
  pxgen <- function(...) {
    processx::process$new(
      px,
      c("outln", "foo", "errln", "bar"),
      stdout = tempfile(),
      stderr = tempfile(),
      ...
    )
  }

  afun <- function() {
    when_all(external_process(pxgen), async_constant(13))
  }

  res <- synchronise(afun())
  expect_equal(res[[1]]$status, 0L)
  expect_match(res[[1]]$stdout, "foo\r?\n")
  expect_match(res[[1]]$stderr, "bar\r?\n")
  expect_false(res[[1]]$timeout)
  expect_equal(res[[2]], 13)
})

test_that("cancel external_process", {
  px <- asNamespace("processx")$get_tool("px")
  proc <- NULL
  pxgen <- function(...) {
    proc <<- processx::process$new(
      px,
      c("sleep", "5"),
      stdout = tempfile(),
      stderr = tempfile(),
      ...
    )
  }

  running <- NULL
  afun <- function() {
    when_all(
      external_process(pxgen),
      delay(0.001)$
        then(function() {
          limit <- Sys.time() + as.difftime(2, units = "secs")
          while (Sys.time() < limit && !proc$is_alive()) Sys.sleep(0.1)
          running <<- proc$is_alive()
        })$
        then(function() stop("failed"))
    )
  }

  expect_error(synchronise(afun()))
  expect_true(running)

  limit <- Sys.time() + as.difftime(2, units = "secs")
  while (Sys.time() < limit && proc$is_alive()) Sys.sleep(0.1)
  expect_false(proc$is_alive())
})

test_that("discarding stdout/stderr works", {
  px <- asNamespace("processx")$get_tool("px")
  pxgen <- function(...) {
    processx::process$new(
      px,
      c("outln", "foo", "errln", "bar"),
      stdout = NULL,
      stderr = NULL,
      ...
    )
  }

  afun <- function() external_process(pxgen)

  res <- synchronise(afun())
  expect_equal(res$status, 0L)
  expect_null(res$stdout)
  expect_null(res$stderr)
  expect_false(res$timeout)
})

test_that("can disable error on status", {
  px <- asNamespace("processx")$get_tool("px")
  pxgen <- function(...) {
    processx::process$new(
      px,
      c("return", "1"),
      ...
    )
  }
  afun <- function(...) external_process(pxgen, ...)

  expect_error(
    synchronise(afun()),
    "exited with non-zero status"
  )

  res <- synchronise(afun(error_on_status = FALSE))
  expect_equal(res, list(
    status = 1L,
    stdout = NULL,
    stderr = NULL,
    timeout = FALSE
  ))
})
