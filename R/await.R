
#' @export

await <- function(ids) {
  assert_that(is.character(ids))
  get_default_event_loop()$await(ids)
}

#' @export

await_all <- function() {
  get_default_event_loop()$await_all()

}

#' @export

await_any <- function(ids) {
  assert_that(is.character(ids))
  get_default_event_loop()$await_any(ids)
}
