
context("await")

test_that("creates a promise as needed", {
  expect_equal(await("foo"), "foo")
})

test_that("suspends suspendable functions", {

  ## This is timing based
  skip("Not working yet")
  skip_on_cran()

  x <- 5
  foo <- async(function() {
    await(set_timeout(40/1000))
    x <<- 7
    await(set_timeout(40/1000))
    x <<- 9
  })

  dx <- foo()
  dy <- set_timeout(20/1000)$
    then(function(value) expect_equal(x, 5))$
    then(function(value) set_timeout(40/1000))$
    then(function(value) expect_equal(x, 7))$
    then(function(value) set_timeout(40/1000))$
    then(function(value) expect_equal(x, 9))

  await_list(dx, dy)
})

test_that("await with multiple tasks", {

  skip_if_offline()

  dx1 <- http_get("https://eu.httpbin.org/get")$then(~ .$status_code)
  dx2 <- http_get("https://eu.httpbin.org/get?q=42")$then(~ .$status_code)

  await_list(dx1, dx2)
  expect_equal(await(dx1), 200)
  expect_equal(await(dx2), 200)
})
