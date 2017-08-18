
#' Wait for some (or all) tasks to finish
#'
#' If `ids` is `NULL` then it waits for all tasks to finish, i.e. the
#' event loop must be completely empty. This is rarely useful.
#'
#' If `ids` is a a charater vector, then it waits for all listed tasks.
#'
#' @param ids A vector of task ids, or `NULL`.
#'
#' @family synchronization functions
#' @export
#' @examples
#' id <- parallel(
#'   list(
#'     function(callback) { Sys.sleep(1/100); callback(NULL, 1) },
#'     function(callback) { Sys.sleep(1/200); callback(NULL, 2) },
#'     function(callback) {
#'       http_get("http://httpbin.org", function(err, res) {
#'         callback(err, res$status_code)
#'       })
#'     }
#'   ),
#'   function(err, res) print(res)
#' )
#' await(id)

await <- function(ids = NULL) {
  assert_that(is.character(ids) || is.null(ids))
  if (is.null(ids)) {
    get_default_event_loop()$await_all()
  } else {
    get_default_event_loop()$await(ids)
  }
}
