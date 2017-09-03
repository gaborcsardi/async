
context("reflect")

test_that("reflect", {

  badfun <- async(function() stop("oh no!"))
  safefun <- reflect(badfun)

  result <- await(when_all(safefun, safefun, safefun))

  for (i in 1:3) {
    expect_s3_class(result[[i]]$error, "error")
    expect_null(result[[i]]$value)
  }
})
