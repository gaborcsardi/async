
context("parallel")

test_that("parallel", {

  skip_if_offline()

  do <- async(function() {
    dx1 <- http_get("https://eu.httpbin.org/get?q=foo")$
      then( ~ rawToChar(.$content))
    dx2 <- http_get("https://eu.httpbin.org/get?q=bar")$
      then( ~ rawToChar(.$content))

    dx1$then(~ expect_match(., "\"q\": \"foo\"", fixed = TRUE))
    dx2$then(~ expect_match(., "\"q\": \"bar\"", fixed = TRUE))
  })
  synchronise(do())
})
