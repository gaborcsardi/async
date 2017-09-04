
context("async_map")

test_that("async_map", {
  list <- structure(as.list(1:10), names = letters[1:10])

  fun <- async(function(x) {
    force(x)
    delay(1/100)$then(function(value) x * 2)
  })

  result <- await(async_map(list, fun))
  expect_identical(result, as.list(unlist(list) * 2))
})
