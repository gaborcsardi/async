
context("errors")

test_that("rejection", {

  do <- async(function() {
    dx <- delay(1/10000)$
      then(function(value) stop("ohno!"))

    expect_error(await(dx), "ohno!")
  })
  sync_wrap(do())
})

test_that("error propagates", {

  do <- async(function() {
    called <- FALSE
    dx <- delay(1/10000)$
      then(function(x) x)$
      then(function(x) stop("ohno!"))$
      then(function(x) called <<- TRUE)

    expect_error(await(dx, "ohno!"))
    expect_false(called)
  })
  sync_wrap(do())
})

test_that("handled error is not an error any more", {

  do <- async(function() {
    dx <- delay(1/10000)$
      then(function(x) stop("ohno!"))$
      then(NULL, function(x) "OK")

    expect_silent(await(dx))
    expect_equal(await(dx), "OK")
  })
  sync_wrap(do())
})

test_that("catch", {
  do <- async(function() {
    dx <- delay(1/1000)$
      then(~ .)$
      then(~ stop("ooops"))$
      then(~ "not this one")$
      catch(~ "nothing to see here")

    expect_equal(await(dx), "nothing to see here")
  })
  sync_wrap(do())
})

test_that("finally", {
  do <- async(function() {
    called <- FALSE
    dx <- delay(1/1000)$
      then(~ .)$
      then(~ stop("oops"))$
      then(~ "not this one")$
      finally(function() called <<- TRUE)

    expect_error(await(dx), "oops")
    expect_true(called)

    called <- FALSE
    dx <- delay(1/1000)$
      then(~ .)$
      then(~ "this one")$
      finally(function() called <<- TRUE)

    expect_equal(await(dx), "this one")
    expect_true(called)
  })
  sync_wrap(do())
})

test_that("errors from other resolutions are not reported", {

  do <- async(function() {
    dx1 <- delay(1/10000)$then(~ stop("wrong"))
    dx2 <- delay(1/10)$then(~ "OK")

    expect_equal(await(dx2), "OK")
    expect_equal(dx1$get_state(), "rejected")
    expect_error(await(dx1), "wrong")

    dx1 <- delay(1/10000)$then(~ stop("wrong"))
    dx2 <- delay(1/10)$then(~ stop("oops"))

    expect_error(await(dx2), "oops")
    expect_equal(dx1$get_state(), "rejected")
    expect_error(await(dx1), "wrong")
  })
  sync_wrap(do())
})
