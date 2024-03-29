
#' Asynchronous HTTP GET request
#'
#' Start an HTTP GET request in the background, and report its completion
#' via a deferred.
#'
#' @section HTTP event emitters:
#' An async HTTP deferred object is also an event emitter, see
#' [event_emitter]. Use `$event_emitter` to access the event emitter API,
#' and call `$event_emitter$listen_on()` etc. to listen on HTTP events,
#' etc.
#'
#' * `"data"` is emitted when we receive data from the server, the data is
#'   passed on to the listeners as a raw vector. Note that zero-length
#'   raw vectors might also happen.
#' * `"end"` is emitted at the end of the HTTP data stream, without
#'   additional arguments (Also on error.)
#'
#' Here is an example, that uses the web server from the webfakes
#' package:
#' ```r
#' http <- webfakes::new_app_process(webfakes::httpbin_app())
#' stream_http <- function() {
#'   query <- http_get(http$url("/drip?duration=3&numbytes=10"))
#'   query$event_emitter$
#'     listen_on("data", function(bytes) {
#'       writeLines(paste("Got", length(bytes), "byte(s):"))
#'       print(bytes)
#'     })$
#'     listen_on("end", function() {
#'       writeLines("Done.")
#'     })
#'   query
#' }
#'
#' response <- synchronise(stream_http())
#' ```
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
#' @examples
#' \donttest{
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/200")$
#'     then(function(x) x$status_code)
#' })
#' synchronise(afun())
#' }

http_get <- function(url, headers = character(), file = NULL,
                     options = list(), on_progress = NULL) {

  url; headers; file; options; on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)

      if (!is.null(on_progress)) {
        options$noprogress <- FALSE
        fun <- options$progressfunction <- function(down, up) {
          on_progress(list(
            url = url,
            handle = handle,
            file = file,
            total = down[[1]],
            current = down[[2]]
          ))
          TRUE
        }
        ## This is a workaround for curl not PROTECT-ing the progress
        ## callback function
        reg.finalizer(handle, function(...) fun, onexit = TRUE)
      }

      curl::handle_setopt(handle, .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_get <- mark_as_async(http_get)

#' Asynchronous HTTP HEAD request
#'
#' An async HTTP deferred object is also an event emitter, see
#' [http_get()] for details, and also [event_emitter].
#'
#' @inheritParams http_get
#' @return Deferred object.
#'
#' @family asyncronous HTTP calls
#' @export
#' @examples
#' \donttest{
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
#' }

http_head <- function(url, headers = character(), file = NULL,
                      options = list(), on_progress = NULL) {

  url; headers; file; options; on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(handle, customrequest = "HEAD", nobody = TRUE,
                    .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_head <- mark_as_async(http_head)

#' Asynchronous HTTP POST request
#'
#' Start an HTTP POST request in the background, and report its completion
#' via a deferred value.
#'
#' An async HTTP deferred object is also an event emitter, see
#' [http_get()] for details, and also [event_emitter].
#'
#' @inheritParams http_get
#' @param data Data to send. Either a raw vector, or a character string
#'   that will be converted to raw with [base::charToRaw]. At most one of
#'   `data`, `data_file` and `data_form` can be non `NULL`.
#' @param data_file Data file to send. At most one of `data`, `data_file`
#'   and `data_form` can be non `NULL`.
#' @param data_form Form data to send. A name list, where each element
#'   is created with either [curl::form_data()] or [curl::form_file()].
#'   At most one of `data`, `data_file` and `data_form` can be non `NULL`.
#' @param on_progress Progress handler function. It is only used if the
#'   response body is written to a file. See details at [http_get()].
#'
#' @export
#' @examples
#' json <- jsonlite::toJSON(list(baz = 100, foo = "bar"))
#'
#' do <- function() {
#'   headers <- c("content-type" = "application/json")
#'   http_post("https://eu.httpbin.org/post", data = json, headers = headers)$
#'     then(http_stop_for_status)$
#'     then(function(x) {
#'       jsonlite::fromJSON(rawToChar(x$content))$json
#'     })
#' }
#'
#' synchronise(do())

http_post <- function(url, data = NULL, data_file = NULL,
                      data_form = NULL, headers = character(), file = NULL,
                      options = list(), on_progress = NULL) {

  url; data; data_file; data_form; headers; file; options; on_progress
  if ((!is.null(data) + !is.null(data_file) + !is.null(data_form)) > 1) {
    stop(
      "At most one of `data`, `data_file` and `data_form` ",
      "can be non `NULL`."
    )
  }
  if (!is.null(data_file)) {
    data <- readBin(data_file, "raw", file.size(data_file))
  }
  if (!is.null(data) && !is.raw(data)) data <- charToRaw(data)
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(handle, customrequest = "POST",
                    postfieldsize = length(data), postfields = data,
                    .list = options)
      if (!is.null(data_form)) {
        curl::handle_setform(handle, .list = data_form)
      }
      list(handle = handle, options = options)
    },
    file
  )
}

http_post <- mark_as_async(http_post)

http_delete <- function(url, headers = character(), file = NULL,
                        options = list()) {
  url; headers; options;

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- curl::new_handle(url = url)
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(handle, customrequest = "DELETE", .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_delete <- mark_as_async(http_delete)

#' @importFrom utils modifyList

get_default_curl_options <- function(options) {
  getopt <- function(nm) {
    if (!is.null(v <- options[[nm]])) return(v)
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return (v)
  }
  modifyList(
    options,
    drop_nulls(list(
      timeout = as.integer(getopt("timeout") %||% 0),
      connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
      low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
      low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0),
      cainfo = getopt("cainfo")
    ))
  )
}

http_events <- R6Class(
  "http_events",
  inherit = event_emitter,
  public = list(
    listen_on = function(event, callback) {
      private$check(event)
      super$listen_on(event, callback)
    },
    listen_off = function(event, callback) {
      private$check(event)
      super$listen_off(event, callback)
    }
  ),
  private = list(
    check = function(event) {
      stopifnot(event %in% c("data", "end"))
    }
  )
)

make_deferred_http <- function(cb, file) {
  cb; file
  id <- NULL
  ee <- http_events$new()
  deferred$new(
    type = "http", call = sys.call(),
    action = function(resolve, progress) {
      resolve; progress
      ## This is a temporary hack until we have proper pollables
      ## Then the deferred will have a "work" callback, which will
      ## be able to throw.
      reject <- environment(resolve)$private$reject
      ho <- cb()
      id <<- get_default_event_loop()$add_http(
        ho$handle,
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        progress,
        file,
        data = c(ho$options, list(event_emitter = ee))
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    },
    event_emitter = ee
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
#' \donttest{
#' afun <- async(function() {
#'   http_get("https://eu.httpbin.org/status/404")$
#'     then(http_stop_for_status)
#' })
#'
#' tryCatch(synchronise(afun()), error = function(e) e)
#' }

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
  if (length(resp[["content"]]) == 0 && !is.null(resp$file) &&
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

#' Set curl HTTP options in an event loop
#'
#' The event loop must be already running. In other words, you can only
#' call this function from async functions.
#'
#' The default values are set when the first deferred HTTP operation of the
#' event loop is created, and they are taken from the `async_http_total_con`,
#' `async_http_host_con` and `async_http_multiplex` options.
#'
#' @param total_con,host_con,multiplex They are passed to
#'   [curl::multi_set()]. If an argument is `NULL` (the default) then it is
#'   ignored.
#' @export
#' @family asyncronous HTTP calls

http_setopt <- function(total_con = NULL, host_con = NULL, multiplex = NULL) {
  get_default_event_loop()$http_setopt(total_con, host_con, multiplex)
  invisible()
}
