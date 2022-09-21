
test_that("process", {

  px <- asNamespace("processx")$get_tool("px")

  afun <- async(function() {
    when_all(
      delay = delay(1/1000)$
        then(function(x) 1),
      process = run_process(px, c("outln", "foobar"))$
        then(function(x) str_trim(x$stdout)),
      r_process = run_r_process(function() 2)$
        then(function(x) x$result)
    )
  })

  res <- synchronise(afun())

  expect_equal(
    res,
    list(delay = 1,
         process = "foobar",
         r_process = 2)
  )
})

test_that("process + http", {
  px <- asNamespace("processx")$get_tool("px")

  afun <- async(function() {
    when_all(
      delay = delay(1/1000)$
        then(function() 1),
      http = http_get(http$url("/status/418"))$
        then(function(x) x$status_code),
      process = run_process(px, c("outln", "foobar"))$
        then(function(x) str_trim(x$stdout)),
      r_process = run_r_process(function() 2)$
        then(function(x) x$result)
    )
  })

  res <- synchronise(afun())

  expect_equal(
    res,
    list(delay = 1,
         http = 418,
         process = "foobar",
         r_process = 2)
  )
})
