
context("each_of")

test_that("each_of", {

  skip("need to rewrite with deferred")
  
  ok <- NULL
  done <- character()
  index <- numeric()

  wait_for(each_of(
    letters[1:10],
    function(item, idx, callback) {
      done <<- c(done, item)
      index <<- c(index, idx)
      callback(NULL)
    },
    function(err) { if (is.null(err)) ok <<- TRUE }
  ))

  expect_true(ok)
  expect_identical(sort(done), sort(letters[1:10]))
})
