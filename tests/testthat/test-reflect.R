
context("async_reflect")

test_that("async_reflect", {

  badfun <- async(function() stop("oh no!"))
  safefun <- async_reflect(badfun)

  do <- async(function() {
    result <- await(when_all(safefun(), safefun(), safefun()))

    for (i in 1:3) {
      expect_s3_class(result[[i]]$error, "error")
      expect_null(result[[i]]$value)
    }
  })
  synchronise(do())
})
