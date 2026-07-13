test_that("fail, success", {
  local_edition(3)
  did <- 0L
  uns <- function() {
    if (did == 3) {
      "answer"
    } else {
      did <<- did + 1
      message("not yet")
      stop("not yet")
    }
  }

  bo <- function(i) 0.1

  # ok
  did <- 0L
  expect_snapshot(
    synchronise(async_backoff(uns, custom_backoff = bo))
  )

  # not ok
  did <- 0L
  expect_snapshot(
    error = TRUE,
    synchronise(async_backoff(uns, custom_backoff = bo, times = 2))
  )

  # time_limit ok
  did <- 0L
  expect_snapshot(
    synchronise(async_backoff(uns, custom_backoff = bo, time_limit = 1))
  )

  # time_limit not ok
  did <- 0L
  uns2 <- function() {
    suppressMessages(uns())
  }
  expect_snapshot(
    error = TRUE,
    synchronise(async_backoff(uns2, custom_backoff = bo, time_limit = 0.1))
  )
})

test_that("progress", {
  local_edition(3)
  did <- 0L
  uns <- function() {
    if (did == 3) {
      "answer"
    } else {
      did <<- did + 1
      message("not yet")
      stop("not yet")
    }
  }

  bo <- function(i) 0.1

  progress <- function(status, data) {
    status$error$call <- status$error$calls <- status$error$aframe <- NULL
    class(status$error) <- setdiff(class(status$error), "async_rejected")
    print(status)
    print(data)
  }

  trfm <- function(x) {
    sub(
      "Time difference of [.0-9]+ secs",
      "Time difference of <some> secs",
      x
    )
  }

  # ok
  did <- 0L
  expect_snapshot(
    synchronise(async_backoff(
      uns,
      custom_backoff = bo,
      on_progress = progress,
      progress_data = "data"
    )),
    transform = trfm
  )

  # not ok
  did <- 0L
  expect_snapshot(
    error = TRUE,
    synchronise(async_backoff(
      uns,
      custom_backoff = bo,
      times = 2,
      on_progress = progress,
      progress_data = "data"
    )),
    transform = trfm
  )
})

test_that("default_backoff", {
  bo <- sapply(1:10, default_backoff)
  expect_true(all(bo >= 1))
  expect_true(all(bo <= 2^(1:10)))
})

test_that("HTTP backoff example", {
  local_edition(3)

  flaky <- webfakes::new_app()
  flaky$get("/unstable", function(req, res) {
    if (identical(res$app$locals$counter, 3L)) {
      res$app$locals$counter <- NULL
      res$send_json(object = list(result = strrep("ok", 100)))
    } else {
      res$app$locals$counter <- c(res$app$locals$counter, 0L)[[1]] + 1L
      res$send_status(401)
    }
  })

  pr <- webfakes::new_app_process(flaky)
  url <- pr$url("/unstable")

  messages <- character()

  cb_http <- function(data) {
    messages <<- c(
      messages,
      sprintf("%s: got %s/%s", url, data$current, data$total)
    )
  }

  cb_backoff <- function(data, url) {
    messages <<- c(
      messages,
      if (data$event == "retry") {
        sprintf("%s failed, retry in %f seconds", url, data$retry_in)
      } else {
        sprintf("%s: given up after %d tries", url, data$tries)
      }
    )
  }

  fun <- function() {
    query <- function(url) {
      http_get(url, on_progress = cb_http)$then(http_stop_for_status)
    }

    async_backoff(
      query,
      .args = list(url = url),
      times = 4,
      on_progress = cb_backoff,
      progress_data = url,
      custom_backoff = function(i) 0.1
    )
  }

  synchronise(fun())

  shift <- function(v) {
    if (length(v) == 0) {
      v
    } else {
      c(v[-1], "")
    }
  }

  uniq <- function(x) {
    x[x != shift(x)]
  }

  expect_snapshot(
    uniq(messages),
    transform = function(x) sub(":[0-9]+", "<port>", x)
  )
})
