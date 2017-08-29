
context("reflect")

test_that("reflect", {

  skip("need to rewrite with deferred")  

  badfun <- asyncify(function() stop("oh no!"))
  safefun <- reflect(badfun)

  result <- NULL
  wait_for(parallel(
    list(safefun, safefun, safefun),
    function(err, res) result <<- res
  ))

  for (i in 1:3) {
    expect_s3_class(result[[i]]$error, "error")
    expect_null(result[[i]]$value)
  }
})
