
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

  afun <- function() {
    x <- async_constant(42)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    x$then(function(.) . + 42)
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_detect", {

  afun <- function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    async_detect(list(x, 2), function(.x) .x$then(function(.) . == 1))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_every", {

  afun <- function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    async_every(list(x, 1), function(.x) .x$then(function(.) . == 1))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})


test_that("synchronization barriers, async_filter", {

  afun <- function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    async_filter(list(x, 1), function(.x) .x$then(function(.) . == 1))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, async_map", {

  afun <- function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    async_filter(list(x, 1), function(.x) .x$then(function(.) . + 1))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, then()", {

  afun <- function() {
    x <- async_constant(1)
    synchronise(afun2(x))
  }

  afun2 <- function(x) {
    x$then(function(v) v + 1)
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, when_all", {

  afun <- async(function() {
    x <- async_constant(1)
    synchronise(afun2(x))
    x
  })

  afun2 <- function(x) {
    when_all(x, async_constant(2))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, when_some", {

  afun <- async(function() {
    x <- async_constant(1)
    get_private(x)$null()
    synchronise(afun2(x))
  })

  afun2 <- function(x) {
    when_some(1, x, async_constant(2))
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")

  afun2 <- function(x) {
    when_some(1, async_constant(2), x)
  }

  expect_error(
    synchronise(afun()),
    class = "async_synchronization_barrier_error")
})

test_that("synchronization barriers, leaked deferred", {

  leak <- NULL
  do <- function() {
    leak <<- async_constant(1)
    leak
  }

  synchronise(do())

  expect_false(is.null(leak))
  expect_error(
    synchronise(leak),
    class = "async_synchronization_barrier_error")
})

test_that("printing async_rejected", {
  do  <- function() {
    delay(1/1000)$then(function() stop("oops"))
  }

  res <- tryCatch(synchronise(do()), error = function(e) e)
  expect_match(
    format(res),
    paste0("<async error: oops\n in *parent* callback of ",
           "`delay(1/1000)$then` at "),
    fixed = TRUE
  )
})

test_that("summary.async_rejected", {
  id <- NULL
  do  <- function() {
    p <- delay(1/1000)$then(function() stop("oops"))
    id <<- get_private(p)$id
    p
  }

  res <- tryCatch(synchronise(do()), error = function(e) e)
  sm <- summary(res)
  fmt <-  format(sm)
  expect_match(fmt, format(res), fixed = TRUE)
  expect_match(fmt, paste0(id, " parent .*stop[(]\"oops\"[)]"))
})
