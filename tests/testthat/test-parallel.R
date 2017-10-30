
context("parallel")

test_that("parallel", {

  skip_if_offline()

  do <- async(function() {
    dx1 <- http_get("https://eu.httpbin.org/get?q=foo")$
      then( ~ rawToChar(.$content))
    dx2 <- http_get("https://eu.httpbin.org/get?q=bar")$
      then( ~ rawToChar(.$content))

    await(dx1)
    await(dx2)

    expect_match(dx1$get_value(), "\"q\": \"foo\"", fixed = TRUE)
    expect_match(dx2$get_value(), "\"q\": \"bar\"", fixed = TRUE)
  })
  sync_wrap(do())
})
