
#' @export

sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  if (length(funcs) == 0) stop("Function list empty in `sequence`")

  function(...) {
    dx <- async(funcs[[1]])(...)
    for (i in seq_along(funcs)[-1]) dx <- dx$then(funcs[[i]])
    dx
  }
}
