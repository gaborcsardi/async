
context("each")

test_that("each", {

  ok <- NULL
  done <- character()

  await(each(
    letters[1:10],
    function(item, callback) { done <<- c(done, item); callback(NULL)  },
    function(err) { if (is.null(err)) ok <<- TRUE }
  ))

  expect_true(ok)
  expect_identical(sort(done), sort(letters[1:10]))
})

test_that("each, asyncify", {

  ok <- NULL
  done <- character()

  await(each(
    letters[1:10],
    asyncify(return = FALSE, function(item) done <<- c(done, item)),
    function(err) { if (is.null(err)) ok <<- TRUE }
  ))

  expect_true(ok)
  expect_identical(sort(done), sort(letters[1:10]))
})
