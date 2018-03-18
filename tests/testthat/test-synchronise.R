
context("synchronise")

test_that("error if async function is called from sync context", {

  mockery::stub(get_default_event_loop, "length", 0)
  expect_error(
    get_default_event_loop(),
    class = "async_synchronization_barrier_error")

  expect_error(
    http_get("url"),
    class = "async_synchronization_barrier_error")
  expect_error(
    async_constant("foobar"),
    class = "async_synchronization_barrier_error")
  expect_error(
    delay(1/1000),
    class = "async_synchronization_barrier_error")

  afun <- async(function() 42)
  expect_error(afun())
})

test_that("simple tests", {

  afun <- async(function() 42)
  expect_equal(
    synchronise(afun()),
    42
  )

  expect_true(synchronise(delay(1/1000)))
})

test_that("synchronization barriers, async_constant", {

  afun <- async(function() {
    x <- async_constant(42)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    x$then(~ . + 42)
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_detect", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    async_detect(list(x, 2), function(.x) .x$then(~ . == 1))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_every", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    async_every(list(x, 1), function(.x) .x$then(~ . == 1))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})


test_that("synchronization barriers, async_filter", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    async_filter(list(x, 1), function(.x) .x$then(~ . == 1))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_map", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    async_filter(list(x, 1), function(.x) .x$then(~ . + 1))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, then()", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    x$then(function(v) v + 1)
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, when_all", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    when_all(x, async_constant(2))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, when_some", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  })

  afun2 <- async(function(x) {
    when_some(1, x, async_constant(2))
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")

  afun2 <- async(function(x) {
    when_some(1, async_constant(2), x)
  })

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})
