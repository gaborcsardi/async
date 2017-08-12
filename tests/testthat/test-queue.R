
context("queue")

test_that("basic queue", {

  hist <- character()

  q <- make_queue(
    function(task, callback) {
      if (task$name == "err") return(callback("ouch", NULL))
      hist <<- c(hist, paste("hello", task$name))
      callback(NULL, NULL)
    },
    concurrency = 2
  )

  expect_equal(q$is_idle(), TRUE)
  q$pause()

  q$call_if_drained(function() {
    hist <<- c(hist, "all items have been processed")
  })

  q$call_if_error(function(err, task) {
    hist <<- c(hist, paste("error", err, task$name))
  })

  q$call_if_empty(function() {
    hist <<- c(hist, "queue empty")
  })

  q$push(list(name = "foo"), function(err, res) {
    hist <<- c(hist, "finished processing foo")
  })

  q$push(list(name = "bar"), function(err, res) {
    hist <<- c(hist, "finished processing bar")
  })

  for (item in c("baz", "bay", "bax")) {
    q$push(list(name = item), function(err, res) {
      hist <<- c(hist, "finished processing item")
    })
  }

  q$unshift(list(name = "bar"), function(err, res) {
    hist <<- c(hist, "finished processing bar")
  })

  q$push(list(name = "err"), function(err, res) {
    hist <<- c(hist, "err is errored")
  })

  expect_equal(q$get_length(), 7)
  expect_equal(q$is_started(), FALSE)
  expect_equal(q$get_concurrency(), 2)
  expect_equal(q$is_paused(), TRUE)
  expect_equal(q$get_running(), 0)
  expect_equal(q$is_idle(), FALSE)

  q$resume()
  expect_equal(q$is_started(), TRUE)
  expect_equal(q$is_paused(), FALSE)

  await_all()

  expect_equal(
    hist,
    c("hello bar", "finished processing bar",
      "hello foo", "finished processing foo",
      "hello bar", "finished processing bar",
      "hello baz", "finished processing item",
      "hello bay", "finished processing item",
      "hello bax", "finished processing item",
      "error ouch err", "err is errored",
      "queue empty",
      "all items have been processed")
  )
})
