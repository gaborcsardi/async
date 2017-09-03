
make_resolved_deferred <- function(x) {
  force(x)
  deferred$new(function(resolve, reject) {
    resolve(x)
  })
}

make_rejected_deferred <- function(x) {
  force(x)
  deferred$new(function(resolve, reject) {
    reject(x)
  })
}

#' @export

async <- function(fun) {
  assert_that(is.function(fun))

  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- expr({
    tryCatch(
      {
        r <- evalq({ !!! body(fun) })
        if (is.deferred(r)) r else make_resolved_deferred(r)
      },
      error = function(e) make_rejected_deferred(e)
    )
  })

  attr(async_fun, "async") <- list(TRUE)

  async_fun
}

#' @export

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(fun, "async")) && identical(a[[1]], TRUE)
}
