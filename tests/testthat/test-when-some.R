test_that("when_some", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1 / 10)$then(function(value) "foo")
    d2 <- delay(1 / 10000)$then(function(value) "bar")

    dx <- when_some(2, d1, d2)$then(function(value) {
      done <<- TRUE
      expect_equal(value, list("bar", "foo"))
    })
  })
  synchronise(do())
  expect_true(done)
})

test_that("when_some, few errors", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1 / 10)$then(function(value) "foo")
    d2 <- delay(1 / 10000)$then(function(.) stop("ooops"))
    d3 <- delay(1 / 10000)$then(function(value) "bar")

    dx <- when_some(2, d1, d2, d3)$then(function(value) {
      done <<- TRUE
      expect_equal(value, list("bar", "foo"))
    })
  })
  synchronise(do())
  expect_true(done)
})

test_that("too many errors", {
  done <- FALSE
  do <- async(function() {
    d1 <- delay(1 / 10)$then(function(.) stop("ooops again"))
    d2 <- delay(1 / 10000)$then(function(.) stop("ooops"))
    d3 <- delay(1 / 10000)$then(function(value) "bar")

    when_some(2, d1, d2, d3)
  })
  err <- tryCatch(synchronise(do()), error = identity)
  expect_equal(conditionMessage(err), "when_some / when_any failed")
  expect_equal(conditionMessage(err$errors[[1]]), "ooops")
  expect_equal(conditionMessage(err$errors[[2]]), "ooops again")
})

test_that("not enough values", {
  do <- async(function() {
    when_some(3, delay(5), delay(5))
  })
  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")

  do2 <- async(function() {
    do()$catch(error = function(.) "repaired")
  })
  expect_equal(synchronise(do2()), "repaired")
})
