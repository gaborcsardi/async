
#' Asynchronous HTTP GET request
#'
#' Start an HTTP GET request in the background, and report its completion
#' via a deferred.
#'
#' @param url URL to connect to.
#' @param headers HTTP headers to send.
#' @param file If not `NULL`, it must be a string, specifying a file.
#'   The body of the response is written to this file.
#' @param options Options to set on the handle. Passed to
#'   [curl::handle_setopt()].
#' @param on_progress Progress handler function. It is only used if the
#'   response body is written to a file. See details below.
#' @return Deferred object.
#'
#' @section Progress bars:
#'
#' `http_get` can report on the progress of the download, via the
#' `on_progress` argument. This is called with a list, with entries:
#' * `url`: the specified url to download
#' * `handle`: the curl handle of the request. This can be queried using
#'   [curl::handle_data()] to get the response status_code, the final
#'   URL (after redirections), timings, etc.
#' * `file`: the `file` argument.
#' * `total`: total bytes of the response. If this is unknown, it is set
#'    to zero.
#' * `current`: already received bytes of the response.
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl new_handle handle_setheaders
#' @examples
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/200")$
#'     then(function(x) x$status_code)
#' })
#' synchronise(afun())

http_get <- function(url, headers = character(), file = NULL,
                     options = list(timeout = 600), on_progress = NULL) {

  url; headers; file; options; on_progress

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- new_handle(url = url)
      handle_setheaders(handle, .list = headers)

      if (!is.null(on_progress)) {
        options$noprogress <- FALSE
        options$progressfunction <- function(down, up) {
          on_progress(list(
            url = url,
            handle = handle,
            file = file,
            total = down[[1]],
            current = down[[2]]
          ))
          TRUE
        }
      }

      handle_setopt(handle, .list = options)
      handle
    },
    file,
    on_progress
  )
}

http_get <- mark_as_async(http_get)

#' Asynchronous HTTP HEAD request
#'
#' @inheritParams http_get
#' @return Deferred object.
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl handle_setopt
#' @examples
#' afun <- async(function() {
#'   dx <- http_head("https://eu.httpbin.org/status/200")$
#'     then(function(x) x$status_code)
#' })
#' synchronise(afun())
#'
#' # Check a list of URLs in parallel
#' afun <- function(urls) {
#'   when_all(.list = lapply(urls, http_head))$
#'     then(function(x) lapply(x, "[[", "status_code"))
#' }
#' urls <- c("https://google.com", "https://eu.httpbin.org")
#' synchronise(afun(urls))

http_head <- function(url, headers = character(), file = NULL,
                      options = list(timeout = 600), on_progress = NULL) {

  url; headers; file; options; on_progress

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- new_handle(url = url)
      handle_setheaders(handle, .list = headers)
      handle_setopt(handle, customrequest = "HEAD", nobody = TRUE,
                    .list = options)
      handle
    },
    file,
    on_progress
  )
}

http_head <- mark_as_async(http_head)

make_deferred_http <- function(cb, file, on_progress) {
  cb; file; on_progress
  id <- NULL
  deferred$new(
    type = "http", call = sys.call(),
    action = function(resolve, progress) {
      resolve; progress
      ## This is a temporary hack until we have proper pollables
      ## Then the deferred will have a "work" callback, which will
      ## be able to throw.
      reject <- environment(resolve)$private$reject
      handle <- cb()
      id <<- get_default_event_loop()$add_http(
        handle,
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        progress,
        file)
    },
    on_progress = on_progress,
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

#' Throw R errors for HTTP errors
#'
#' Status codes below 400 are considered successful, others will trigger
#' errors. Note that this is different from the `httr` package, which
#' considers the 3xx status code errors as well.
#'
#' @param resp HTTP response from [http_get()], [http_head()], etc.
#' @return The HTTP response invisibly, if it is considered successful.
#'   Otherwise an error is thrown.
#'
#' @export
#' @examples
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/404")$
#'     then(http_stop_for_status)
#' })
#'
#' tryCatch(synchronise(afun()), error = function(e) e)

http_stop_for_status <- function(resp) {
  if (!is.integer(resp$status_code)) stop("Not an HTTP response")
  if (resp$status_code < 400) return(invisible(resp))
  stop(http_error(resp))
}

http_error <- function(resp, call = sys.call(-1)) {
  status <- resp$status_code
  reason <- http_status(status)$reason
  message <- sprintf("%s (HTTP %d).", reason, status)
  status_type <- (status %/% 100) * 100
  if (is.null(resp$content) && !is.null(resp$file) &&
              file.exists(resp$file)) {
    tryCatch({
      n <- file.info(resp$file, extra_cols = FALSE)$size
      resp$content <- readBin(resp$file, what = raw(), n = n)
    }, error = identity)
  }
  http_class <- paste0("async_http_", unique(c(status, status_type, "error")))
  structure(
    list(message = message, call = call, response = resp),
    class = c(http_class, "error", "condition")
  )
}

http_status <- function(status) {
  status_desc <- http_statuses[as.character(status)]
  if (is.na(status_desc)) {
    stop("Unknown http status code: ", status, call. = FALSE)
  }

  status_types <- c("Information", "Success", "Redirection", "Client error",
    "Server error")
  status_type <- status_types[[status %/% 100]]

  # create the final information message
  message <- paste(status_type, ": (", status, ") ", status_desc, sep = "")

  list(
    category = status_type,
    reason = status_desc,
    message = message
  )
}

http_statuses <- c(
  "100" = "Continue",
  "101" = "Switching Protocols",
  "102" = "Processing (WebDAV; RFC 2518)",
  "200" = "OK",
  "201" = "Created",
  "202" = "Accepted",
  "203" = "Non-Authoritative Information",
  "204" = "No Content",
  "205" = "Reset Content",
  "206" = "Partial Content",
  "207" = "Multi-Status (WebDAV; RFC 4918)",
  "208" = "Already Reported (WebDAV; RFC 5842)",
  "226" = "IM Used (RFC 3229)",
  "300" = "Multiple Choices",
  "301" = "Moved Permanently",
  "302" = "Found",
  "303" = "See Other",
  "304" = "Not Modified",
  "305" = "Use Proxy",
  "306" = "Switch Proxy",
  "307" = "Temporary Redirect",
  "308" = "Permanent Redirect (experimental Internet-Draft)",
  "400" = "Bad Request",
  "401" = "Unauthorized",
  "402" = "Payment Required",
  "403" = "Forbidden",
  "404" = "Not Found",
  "405" = "Method Not Allowed",
  "406" = "Not Acceptable",
  "407" = "Proxy Authentication Required",
  "408" = "Request Timeout",
  "409" = "Conflict",
  "410" = "Gone",
  "411" = "Length Required",
  "412" = "Precondition Failed",
  "413" = "Request Entity Too Large",
  "414" = "Request-URI Too Long",
  "415" = "Unsupported Media Type",
  "416" = "Requested Range Not Satisfiable",
  "417" = "Expectation Failed",
  "418" = "I'm a teapot (RFC 2324)",
  "420" = "Enhance Your Calm (Twitter)",
  "422" = "Unprocessable Entity (WebDAV; RFC 4918)",
  "423" = "Locked (WebDAV; RFC 4918)",
  "424" = "Failed Dependency (WebDAV; RFC 4918)",
  "424" = "Method Failure (WebDAV)",
  "425" = "Unordered Collection (Internet draft)",
  "426" = "Upgrade Required (RFC 2817)",
  "428" = "Precondition Required (RFC 6585)",
  "429" = "Too Many Requests (RFC 6585)",
  "431" = "Request Header Fields Too Large (RFC 6585)",
  "444" = "No Response (Nginx)",
  "449" = "Retry With (Microsoft)",
  "450" = "Blocked by Windows Parental Controls (Microsoft)",
  "451" = "Unavailable For Legal Reasons (Internet draft)",
  "499" = "Client Closed Request (Nginx)",
  "500" = "Internal Server Error",
  "501" = "Not Implemented",
  "502" = "Bad Gateway",
  "503" = "Service Unavailable",
  "504" = "Gateway Timeout",
  "505" = "HTTP Version Not Supported",
  "506" = "Variant Also Negotiates (RFC 2295)",
  "507" = "Insufficient Storage (WebDAV; RFC 4918)",
  "508" = "Loop Detected (WebDAV; RFC 5842)",
  "509" = "Bandwidth Limit Exceeded (Apache bw/limited extension)",
  "510" = "Not Extended (RFC 2774)",
  "511" = "Network Authentication Required (RFC 6585)",
  "598" = "Network read timeout error (Unknown)",
  "599" = "Network connect timeout error (Unknown)"
)
