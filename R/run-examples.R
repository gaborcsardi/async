# Should the examples run?
#
# Internal helper for the manual pages. Returns `TRUE` if the examples
# should run, i.e. if the `NOT_CRAN` environment variable is set to `true`.
# Most examples use a local `webfakes` web server, so they also need
# `webfakes` to be installed.

run_examples <- function() {
  tolower(Sys.getenv("NOT_CRAN")) == "true" &&
    requireNamespace("webfakes", quietly = TRUE)
}
