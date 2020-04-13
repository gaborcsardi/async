
skip_without_package <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    skip(paste0("Needs ", package, " package"))
  } else if (!is.null(version) && packageVersion(package) < version) {
    skip(paste0("Needs ", package, ", version ", version, " at least"))
  }
}

get_private <- function(x) x$.__enclos_env__$private
