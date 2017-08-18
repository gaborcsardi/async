
context("amap")

test_that("amap", {

  result <- NULL
  list <- structure(as.list(1:10), names = letters[1:10])

  wait_for(amap(
    list,
    function(item, callback) { callback(NULL, item * 2) },
    function(err, res) { result <<- res }
  ))

  expect_identical(result, as.list(1:10 * 2))
})

test_that("amap and asyncify", {

  result <- NULL

  wait_for(amap(
    1:10,
    asyncify(function(item) item * 2),
    function(err, res) result <<- res
  ))

  expect_identical(result, as.list(1:10 * 2))
})
