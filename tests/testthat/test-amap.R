
context("async_map")

test_that("async_map", {
  list <- structure(as.list(1:10), names = letters[1:10])

  fun <- async(function(x) {
    force(x)
    delay(1/100)$then(function(value) x * 2)
  })

  result <- wait_for(async_map(list, fun))
  expect_identical(result, as.list(unlist(list) * 2))
})

test_that("async_map with limit", {

  list <- structure(as.list(1:10), names = letters[1:10])
  fun <- async(function(x) {
    force(x)
    delay(1/10000)$then(function(value) x * 2)
  })

  for (l in 1:10) {
    result <- wait_for(async_map(list, fun, .limit = l))
    expect_identical(result, as.list(unlist(list) * 2))
  }
})

test_that("async_map with limit, error", {

  list <- structure(as.list(1:10), names = letters[1:10])
  fun <- async(function(x) {
    force(x)
    delay(1/10000)$then(~ if (x == 7) stop("oops") else x * 2)
  })

  for (l in 1:10) {
    expect_error(wait_for(async_map(list, fun, .limit = l)), "oops")
  }
})
