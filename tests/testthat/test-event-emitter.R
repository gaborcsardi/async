
context("event_emitter")

test_that("can create event emitter", {
  do <- function() {
    x <- event_emitter$new(async = FALSE)
  }
  expect_silent(run_event_loop(do()))
})

test_that("can add a listener", {
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() {  })
  }
  expect_silent(run_event_loop(do()))
})

test_that("can add a one-shot listener", {
  do <- function() {
    x <- event_emitter$new()
    x$listen_once("foo", function() {  })
  }
  expect_silent(run_event_loop(do()))
})

test_that("no listeners", {
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$emit("foo")
  }
  expect_silent(run_event_loop(do()))
})

test_that("listener is called on event", {
  called <- FALSE
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() { called <<- TRUE })
    x$emit("foo")
  }
  expect_silent(run_event_loop(do()))
  expect_true(called)
})

test_that("listener called multiple times", {
  called <- 0L
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() { called <<- called + 1L })
    x$emit("foo")
    x$emit("foo")
    x$emit("foo")
  }
  expect_silent(run_event_loop(do()))
  expect_equal(called, 3L)
})

test_that("listener gets arguments", {
  arg1 <- arg2 <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function(a1, a2) {
      arg1 <<- a1
      arg2 <<- a2
    })
    x$emit("foo", 1, 2)
  }
  expect_silent(run_event_loop(do()))
  expect_equal(arg1, 1)
  expect_equal(arg2, 2)
})

test_that("named arguments to listeners", {
  arg1 <- arg2 <- arg3 <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function(a1, a2, a3) {
      arg1 <<- a1
      arg2 <<- a2
      arg3 <<- a3
    })
    x$emit("foo", a3 = 3, a2 = 2, 1)
  }
  expect_silent(run_event_loop(do()))
  expect_equal(arg1, 1)
  expect_equal(arg2, 2)
  expect_equal(arg3, 3)
})

test_that("all listeners are called", {
  arg1 <- arg2 <- arg3 <- NULL
  arg11 <- arg21 <- arg31 <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function(a1, a2, a3) {
      arg1 <<- a1
      arg2 <<- a2
      arg3 <<- a3
    })
    x$listen_on("foo", function(a1, a2, a3) {
      arg11 <<- a1
      arg12 <<- a2
      arg13 <<- a3
    })
    x$emit("foo", a3 = 3, a2 = 2, 1)
  }
  expect_silent(run_event_loop(do()))
  expect_equal(arg1, 1)
  expect_equal(arg2, 2)
  expect_equal(arg3, 3)
  expect_equal(arg11, 1)
  expect_equal(arg12, 2)
  expect_equal(arg13, 3)
})

test_that("one shot listener is only called once", {
  called <- called1 <- 0L
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() { called <<- called + 1L })
    x$listen_once("foo", function() { called1 <<- called1 + 1L })
    x$emit("foo")
    x$emit("foo")
    x$emit("foo")
  }
  expect_silent(run_event_loop(do()))
  expect_equal(called, 3L)
  expect_equal(called1, 1L)
})

test_that("can remove listener", {
  called <- called2 <- 0L
  cb1 <- function() { called <<- called + 1L }
  cb2 <- function(x = 1) { called2 <<- called2 + 1L }
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", cb1)
    x$listen_on("foo", cb2)
    x$emit("foo")
    x$listen_off("foo", cb2)
    x$emit("foo")
    x$emit("foo")
    x$listen_off("foo", cb1)
    x$emit("foo")
  }

  expect_silent(run_event_loop(do()))
  expect_equal(called, 3L)
  expect_equal(called2, 1L)
})

test_that("only removes one listener instance", {
  called <- 0L
  cb <- function() { called <<- called + 1L }
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", cb)
    x$listen_on("foo", cb)
    x$emit("foo")                       # + 2
    x$listen_off("foo", cb)
    x$emit("foo")                       # + 1
    x$emit("foo")                       # + 1
    x$listen_off("foo", cb)
    x$emit("foo")                       # + 0
  }

  expect_silent(run_event_loop(do()))
  expect_equal(called, 4L)
})

test_that("multiple events", {
  foo <- bar <- FALSE
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() { foo <<- TRUE })
    x$listen_on("bar", function() { bar <<- TRUE })
    x$emit("foo")
    x$emit("bar")
  }
  expect_silent(run_event_loop(do()))
  expect_true(foo)
  expect_true(bar)
})

test_that("list event names", {
  n1 <- n2 <- n3 <- n4 <- n5 <- 0L
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    n1 <<- x$get_event_names()
    cb1 <- function() { foo <<- TRUE }
    cb2 <- function() { bar <<- TRUE }
    x$listen_on("foo", cb1)
    n2 <<- x$get_event_names()
    x$listen_on("bar", cb2)
    n3 <<- x$get_event_names()
    x$listen_off("foo", cb1)
    n4 <<- x$get_event_names()
    x$listen_off("bar", cb2)
    n5 <<- x$get_event_names()
  }
  expect_silent(run_event_loop(do()))
  expect_equal(n1, character())
  expect_equal(n2, "foo")
  expect_equal(n3, c("foo", "bar"))
  expect_equal(n4, "bar")
  expect_equal(n5, character())
})

test_that("get listener count for event", {
  n1 <- n2 <- n3 <- n4 <- n5 <- 0L
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    cb <- function() { foo <<- TRUE }
    n1 <<- x$get_listener_count("foo")
    x$listen_on("foo", cb)
    n2 <<- x$get_listener_count("foo")
    x$listen_on("foo", cb)
    n3 <<- x$get_listener_count("foo")
    x$listen_off("foo", cb)
    n4 <<- x$get_listener_count("foo")
    x$listen_off("foo", cb)
    n5 <<- x$get_listener_count("foo")
  }
  expect_silent(run_event_loop(do()))
  expect_equal(n1, 0L)
  expect_equal(n2, 1L)
  expect_equal(n3, 2L)
  expect_equal(n4, 1L)
  expect_equal(n5, 0L)
})

test_that("remove all listeners", {
  called <- FALSE
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() called <<- TRUE)
    x$listen_on("foo", function() called <<- TRUE)
    x$listen_on("foo", function() called <<- TRUE)
    x$listen_on("foo", function() called <<- TRUE)
    x$remove_all_listeners("foo")
    x$emit("foo")
    x$emit("foo")
    x$emit("foo")
  }
  expect_silent(run_event_loop(do()))
  expect_false(called)
})

test_that("error callback is called on error", {
  err <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() stop("foobar"))
    x$listen_on("error", function(e) err <<- e)
    x$emit("foo")
  }

  expect_silent(run_event_loop(do()))
  expect_false(is.null(err))
  expect_s3_class(err, "error")
  expect_equal(err$event, "foo")
  expect_equal(conditionMessage(err), "foobar")
})

test_that("fail stage if no error callback", {
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() stop("foobar"))
    x$emit("foo")
  }

  expect_error(run_event_loop(do()), "foobar")
})

test_that("all error callbacks are called", {
  err1 <- err2 <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() stop("foobar"))
    x$listen_on("error", function(e) err1 <<- e)
    x$listen_on("error", function(e) err2 <<- e)
    x$emit("foo")
  }

  expect_silent(run_event_loop(do()))
  expect_false(is.null(err1))
  expect_false(is.null(err2))
  expect_s3_class(err1, "error")
  expect_equal(err1$event, "foo")
  expect_equal(conditionMessage(err1), "foobar")
  expect_s3_class(err2, "error")
  expect_equal(err2$event, "foo")
  expect_equal(conditionMessage(err2), "foobar")
})

test_that("error within error callback", {
  err <- NULL
  do <- function() {
    x <- event_emitter$new(async = FALSE)
    x$listen_on("foo", function() stop("foobar"))
    x$listen_on("error", function(e) { err <<- e; stop("baz")  })
    x$emit("foo")
  }

  expect_error(run_event_loop(do()), "baz")
  expect_false(is.null(err))
  expect_s3_class(err, "error")
  expect_equal(conditionMessage(err), "foobar")
})
