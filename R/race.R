
#' A deferred value that resolves when the specified number of deferred
#' values resolve, or is rejected when one of them is rejected
#'
#' These functions are similar to [when_some()] and [when_any()], but they
#' do not ignore errors. If a deferred is rejected, then `race_some()` and
#' `race()` are rejected as well.
#'
#' `race()` is a special case of `count = `: it resolves or is rejected
#' as soon as one deferred resolves or is rejected.
#'
#' async has auto-cancellation, so if the required number of deferred values
#' are resolved, or any deferred value is rejected, the rest are cancelled.
#'
#' @param count Number of deferred values that need to resolve.
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @export

race_some <- function(count, ..., .list = list()) {
  when_some_internal(count, ..., .list = .list, .race = TRUE)
}

race_some <- mark_as_async(race_some)

#' @export
#' @rdname race_some

race <- function(..., .list = list()) {
  when_some_internal(1L, ..., .list = .list, .race = TRUE)$
    then(function(x) x[[1]])
}

race <- mark_as_async(race)
