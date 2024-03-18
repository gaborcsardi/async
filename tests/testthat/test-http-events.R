test_that("end event", {
  done <- NULL
  do <- function() {
    p1 <- http_get(http$url("/get"))
    p2 <- p1$then(function() done <<- c(done, "done"))
    p1$event_emitter$listen_on("end", function(...) {
      done <<- c(done, "end")
    })
    p2
  }

  synchronise(do())
  expect_equal(done, c("end", "done"))
})

test_that("data event", {
  skip_on_cran()
  chunks <- NULL
  called <- 0L
  do <- function() {
    p1 <- http_get(http$url("/drip?duration=1&numbytes=10"))
    p1$event_emitter$listen_on("data", function(bytes) {
      chunks <<- c(chunks, list(bytes))
    })
    p1$event_emitter$listen_on("data", function(bytes) {
      called <<- called + 1L
    })
    p1
  }

  # there is an extra zero-length chunk currently, but let's not
  # rely on that
  synchronise(do())
  expect_true(length(chunks) >= 10)
  expect_equal(length(unlist(chunks)), 10)
  expect_true(called >= 10)
})

test_that("multiple end events", {
  done <- NULL
  do <- function() {
    p1 <- http_get(http$url("/get"))
    p2 <- p1$then(function() done <<- c(done, "done"))
    p1$event_emitter$listen_on("end", function(...) {
      done <<- c(done, "end1")
    })
    p1$event_emitter$listen_on("end", function(...) {
      done <<- c(done, "end2")
    })
    p2
  }

  synchronise(do())
  expect_equal(done, c("end1", "end2", "done"))
})
