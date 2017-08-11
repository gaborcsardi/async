
#' @export

cancel <- function(ids) {
  get_default_event_loop()$cancel(ids)
}

#' @export

cancel_all <- function() {
  get_default_event_loop()$cancel_all()
}
