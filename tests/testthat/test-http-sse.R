test_that("parse_sse_event", {
  testthat::local_edition(3)

  txt <- "foo: bar"
  expect_snapshot(parse_sse_event(charToRaw(txt)))

  txt <- "foo:bar"
  expect_snapshot(parse_sse_event(charToRaw(txt)))

  txt <- "foo: bar\nbaz:foobar\nand:last"
  expect_snapshot(parse_sse_event(charToRaw(txt)))
})

test_that("chunk_sse_events", {
  testthat::local_edition(3)

  # no events yet
  txt <- "foo: bar\nbaz: foo\n"
  expect_snapshot(chunk_sse_events(charToRaw(txt)))

  txt <- "foo: bar\nbaz: foobar\n\nanother: event\n\nand:another\n\n"
  expect_snapshot(chunk_sse_events(charToRaw(txt)))

  # slightly bad separators
  txt <- paste0(
    "\n\n\n",
    "foo: bar\nbaz: foobar",
    "\n\n\n\n\n",
    "another: event",
    "\n\n",
    "and:another",
    "\n\n"
  )
  expect_snapshot(chunk_sse_events(charToRaw(txt)))

  # incomplete last event
  txt <- "foo: bar\nbaz: foobar\n\nanother: event\n\nand:another\n"
  expect_snapshot(chunk_sse_events(charToRaw(txt)))
})

test_that("sse", {
  testthat::local_edition(3)
  server <- webfakes::new_app_process(sseapp())
  url <- server$url("/sse")

  events <- NULL
  do <- function() {
    p1 <- http_get(url)
    evs <- sse_events$new(p1)
    evs$listen_on("event", function(ev) {
      events <<- c(events, list(ev))
    })
    p1
  }

  synchronise(do())
  expect_snapshot(events)
})
