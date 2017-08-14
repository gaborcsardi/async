
context("each")

test_that("each", {

  ok <- NULL
  done <- character()

  await(each(
    letters[1:10],
    function(item, cb) { done <<- c(done, item); cb(NULL)  },
    function(err) { if (is.null(err)) ok <<- TRUE }
  ))

  expect_true(ok)
  expect_identical(sort(done), sort(letters[1:10]))
})
