
httpbin_url <- function() {
  "eu.httpbin.org"
}

is_offline <- (function() {
  offline <- NULL
  function() {
    if (is.null(offline)) {
      offline <<- tryCatch(
        is.na(pingr::ping_port(httpbin_url(), port = 443, count = 1L)),
        error = function(e) TRUE
      )
    }
    offline
  }
})()

skip_if_offline <- function() {
  skip_on_cran()
  if (is_offline()) skip("Offline")
}

skip_without_package <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    skip(paste0("Needs ", package, " package"))
  } else if (!is.null(version) && packageVersion(package) < version) {
    skip(paste0("Needs ", package, ", version ", version, " at least"))
  }
}

get_private <- function(x) x$.__enclos_env__$private
