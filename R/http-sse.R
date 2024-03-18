#' HTTP event emitter for server-sent events
#'
#' Server-sent events are a technique to stream events from a web server
#' to a client, through an open HTTP connection.
#'
#' This class implements an event emitter on an async HTTP query created
#' with [http_get()] and friends, that fires an `"event"` event when the
#' server sends an event. An `"end"` event is emitted when the server
#' closes the connection.
#'
#' An event is a named character vector, the names are the keys of the
#' events.
#'
#' Example using our built-in toy web app:
#' ```r
#' http <- webfakes::new_app_process(async:::sseapp())
#' stream_events <- function() {
#'   query <- http_get(http$url("/sse"))
#'   sse <- sse_events$new(query)
#'   sse$
#'     listen_on("event", function(event) {
#'       writeLines("Got an event:")
#'       print(event)
#'     })$
#'     listen_on("end", function() {
#'       writeLines("Done.")
#'     })
#'   query
#' }
#'
#' response <- synchronise(stream_events())
#' ```
#'
#'
#' @export

sse_events <- R6Class(
  "sse_events",
  inherit = event_emitter,
  public = list(
    initialize = function(http_handle) {
      super$initialize()
      http_handle$event_emitter$listen_on("data", function(bytes) {
        private$data <- c(private$data, bytes)
        private$emit_events()
      })
      http_handle$event_emitter$listen_on("end", function() {
        self$emit("end")
      })
    }
  ),

  private = list(
    data = NULL,
    sep = as.raw(c(0xaL, 0xaL)),
    emit_events = function() {
      evs <- chunk_sse_events(private$data, private$sep)
      private$data <- evs$rest
      for (ev in evs$events) {
        self$emit("event", ev)
      }
    }
  )
)

chunk_sse_events <- function(data, sep = NULL) {
  # skip leading \n
  no <- 0L
  while (no <= length(data) && data[no + 1] == 0x0a) {
    no <- no + 1L
  }
  if (no > 0) {
    data <- data[(no + 1L):length(data)]
  }
  sep <- sep %||% as.raw(c(0xaL, 0xaL))
  mtch <- grepRaw(sep, data, fixed = TRUE, all = TRUE)
  # shortcut for no events
  if (length(mtch) == 0) {
    return(list(events = list(), rest = data))
  }

  events <- vector("list", length(mtch))
  for (p in seq_along(mtch)) {
    from <- if (p == 1) 1L else mtch[p - 1] + 2L
    to <- mtch[p] - 1L
    events[[p]] <- parse_sse_event(data[from:to])
  }
  events <- drop_nulls(events)

  restfrom <- mtch[length(mtch)] + 2L
  rest <- if (restfrom <= length(data)) {
    data[restfrom:length(data)]
  } else {
    raw()
  }
  list(events = events, rest = rest)
}

parse_sse_event <- function(data) {
  txt <- rawToChar(data)
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[lines != ""]
  if (length(lines) == 0) {
    return(NULL)
  }
  keys <- sub(":.*$", "", lines)
  vals <- sub("^[^:]*:[ ]*", "", lines)
  structure(vals, names = keys)
}

drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

sseapp <- function() {
  app <- webfakes::new_app()
  app$get("/sse", function(req, res) {
    `%||%` <- function(l, r) if (is.null(l)) r else l
    if (is.null(res$locals$sse)) {
      duration <- as.double(req$query$duration %||% 2)
      delay <- as.double(req$query$delay %||% 0)
      numevents <- as.integer(req$query$numevents %||% 5)
      pause <- max(duration / numevents, 0.01)
      res$locals$sse <- list(
        sent = 0,
        numevents = numevents,
        pause = pause
      )

      res$
        set_header("cache-control", "no-cache")$
        set_header("content-type", "text/event-stream")$
        set_header("access-control-allow-origin", "*")$
        set_header("connection", "keep-alive")$
        set_status(200)

      if (delay > 0) {
        return(res$delay(delay))
      }
    }

    msg <- paste0(
      "event: ", res$locals$sse$sent + 1L, "\n",
      "message: live long and prosper\n\n"
    )
    res$locals$sse$sent <- res$locals$sse$sent + 1L
    res$write(msg)

    if (res$locals$sse$sent == res$locals$sse$numevents) {
      res$send("")
    } else {
      res$delay(res$locals$sse$pause)
    }
  })
}
