
test_that("parallel", {

  do <- async(function() {
    dx1 <- http_get(http$url("/get", query = list(q = "foo")))$
      then( function(.) rawToChar(.$content))
    dx2 <- http_get(http$url("/get", query = list(q = "bar")))$
      then( function(.) rawToChar(.$content))

    when_all(
      dx1$then(function(.) expect_match(., "\"q\":[ ]*\"foo\"")),
      dx2$then(function(.) expect_match(., "\"q\":[ ]*\"bar\""))
    )
  })
  synchronise(do())
})
