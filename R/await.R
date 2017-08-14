
#' @export

await <- function(ids = NULL) {
  assert_that(is.character(ids) || is.null(ids))
  if (is.null(ids)) {
    get_default_event_loop()$await_all()
  } else {
    get_default_event_loop()$await(ids)
  }
}

#' @export

await_any <- function(ids) {
  assert_that(is.character(ids))
  get_default_event_loop()$await_any(ids)
}
