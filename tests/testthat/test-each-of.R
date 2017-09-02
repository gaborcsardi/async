
context("each_of")

test_that("each_of", {

  done <- character()
  index <- integer()

  coll <- letters[1:10]

  dx <- when_all(
    .list = lapply(seq_along(coll), function(i) {
      delay(1/1000)$then(function(value) {
        done <<- c(done, coll[[i]])
        index <<- c(index, i)
      })
    })
  )$then(function(value) {
    expect_identical(sort(index), seq_along(coll))
    expect_identical(sort(done), sort(coll))
  })
})
