
context("async_every")

test_that("async_every", {

  is_odd <- async(
    function(x) delay(1/1000)$then(function(value) as.logical(x %% 2))
  )

  do <- async(function() {
    async_every(1:10, is_odd)$
      then(~ expect_identical(., FALSE))

    async_every(numeric(), is_odd)$
      then(~ expect_identical(., TRUE))

    async_every(1:10 * 2 + 1, is_odd)$
      then(~ expect_identical(., TRUE))
  })
  synchronise(do())
})
