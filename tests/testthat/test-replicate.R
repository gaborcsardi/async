
test_that("0 times", {
  do <- function() async_replicate(0, function() stop("doh"))
  expect_equal(synchronise(do()), list())
})

test_that("async_replicate", {
  do <- function(limit) {
    async_replicate(10, function() runif(1), .limit = limit)
  }

  for (lim in c(1, 2, 5, 10, 20, Inf)) {
    res <- synchronise(do(limit = lim))
    expect_equal(length(res), 10)
  }
})
