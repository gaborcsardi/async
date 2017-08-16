
context("each_of")

test_that("each_of", {

  ok <- NULL
  done <- character()
  index <- numeric()

  await(each_of(
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
