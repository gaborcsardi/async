
#' @export

await <- function(ids) {
  get_default_event_loop()$await(ids)
}

#' @export

await_all <- function() {
  get_default_event_loop()$await_all()

}

#' @export

await_any <- function(ids) {
  get_default_event_loop()$await_any(ids)
}
