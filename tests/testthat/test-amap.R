
context("amap")

test_that("amap", {

  result <- NULL
  list <- structure(as.list(1:10), names = letters[1:10])

  amap(
    list,
    function(item, cb) { cb(NULL, item * 2) },
    function(err, res) { result <<- res }
  )

  await_all()
  expect_identical(
    result,
    structure(as.list(1:10 * 2), names = names(list))
  )
})
