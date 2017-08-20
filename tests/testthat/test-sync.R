
context("sync")

test_that("sync", {

  result <- sync(
    parallel,
    list(
      asyncify(function() 1L),
      function(callback) callback(NULL, 2L),
      asyncify(function() 3L)
    )
  )

  expect_equal(result, list(1L, 2L, 3L))
})

test_that("syncify", {

  sync_parallel <- syncify(parallel)

  result <- sync_parallel(list(
    asyncify(function() 1L),
    function(callback) callback(NULL, 2L),
    asyncify(function() 3L)
  ))

  expect_equal(result, list(1L, 2L, 3L))
})
