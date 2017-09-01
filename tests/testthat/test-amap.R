
context("amap")

test_that("amap", {
  list <- structure(as.list(1:10), names = letters[1:10])

  fun <- async(function(x) {
    delay(1/10)$then(function(value) x * 2)
  })

  result <- await_list(.list = lapply(list, fun))
  expect_identical(result, as.list(unlist(list) * 2))
})
