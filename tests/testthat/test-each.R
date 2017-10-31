
context("each")

test_that("each", {

  do <- async(function() {
    done <- character()
    dx <- when_all(
      .list = lapply(letters[1:10], function(x) {
        delay(1/1000)$then(function(value) done <<- c(done, x))
      })
    )$
    then(function(value) expect_identical(sort(done), sort(letters[1:10])))

    await(dx)
  })
  synchronise(do())
})
