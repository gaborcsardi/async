
context("each")

test_that("each", {

  skip("need to rewrite with deferred")
  
  ok <- NULL
  done <- character()

  wait_for(each(
    letters[1:10],
    function(item, callback) { done <<- c(done, item); callback(NULL)  },
    function(err) { if (is.null(err)) ok <<- TRUE }
  ))

  expect_true(ok)
  expect_identical(sort(done), sort(letters[1:10]))
})
