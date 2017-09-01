
context("await")

test_that("creates a promise as needed", {
  expect_equal(await("foo"), "foo")
})

test_that("multiple async functions run in parallel", {

  ## This is timing based
  skip_on_cran()

  x <- 5
  foo <- async(function() {
    await(delay(40/100))
    x <<- 7
    await(delay(40/100))
    x <<- 9
  })

  dy <- delay(20/100)$
    then(function(value) expect_equal(x, 5))$
    then(function(value) delay(40/100))$
    then(function(value) expect_equal(x, 7))$
    then(function(value) delay(40/100))$
    then(function(value) expect_equal(x, 9))
  dx <- foo()

  await_list(dx, dy)
})

test_that("resumes with the value of the awaited expression", {
  foo <- async(function() {
    await(delay(1/1000)$then(function(value) "blah"))
  })

  dx <- foo()$
    then(function(result) expect_equal(result, "blah"))

  await(dx)
})

test_that("throws into the suspendable function", {
  foo <- async(function() {
    await(delay(1/1000)$then(function(value) stop("blah")))
  })

  dx <- foo()$
    then(NULL, function(reason) expect_equal(reason$message, "blah"))

  await(dx)
})

test_that("resumes with all values of the awaited expressions", {
  foo <- async(function() {
    await(delay(1/1000)$then(function(value) "foo"))
  })
  bar <- async(function() {
    await(delay(2/1000)$then(function(value) "bar"))
  })
  all <- async(function() await_list(foo(), bar()))

  dx <- all()$
    then(function(result) expect_equal(result, list("foo", "bar")))

  await(dx)
})

test_that("throws into the suspendable function the first error", {

  skip("Does not work currently")

  foo <- async(function() {
    delay(1)$then(function(value) stop("foo"))
  })
  bar <- async(function() {
    delay(1/100)$then(function(value) stop("bar"))
  })
  all <- async(function() await_list(foo(), bar()))

  dx <- all()$
    then(NULL, function(reason) expect_equal(reason$message, "bar"))

  await(dx)
})

test_that("await with multiple tasks", {

  skip_if_offline()

  dx1 <- http_get("https://eu.httpbin.org/get")$then(~ .$status_code)
  dx2 <- http_get("https://eu.httpbin.org/get?q=42")$then(~ .$status_code)

  await_list(dx1, dx2)
  expect_equal(await(dx1), 200)
  expect_equal(await(dx2), 200)
})
