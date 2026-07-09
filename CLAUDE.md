# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`async` is an R package that brings asynchronous computation and I/O to R. It
provides an event loop that schedules async functions whose results are reported
via **deferred values** (promises/futures). Deferred values can be chained and are
evaluated lazily, at synchronisation points.

## Common commands

This is a standard R package developed with `devtools`/`testthat` (edition 3).

```r
# Load the package for interactive development
uncovr::reload()

# Run the full test suite
uncovr::test()

# Run a single test file (by filename fragment, no "test-" prefix or ".R")
uncovr::test(filter = "http")          # runs tests/testthat/test-http.R

# Documentation (roxygen2 -> man/, NAMESPACE)
uncovr::document()

# R CMD check
withr::with_envvar(c(NOT_CRAN = "true", DISPLAY = NA), rcmdcheck::rcmdcheck())
```

```sh
# Regenerate README.md from README.Rmd (the only Makefile target)
make
```

Documentation is roxygen-based (`Roxygen: list(markdown = TRUE, r6 = FALSE)`).
Note `r6 = FALSE`: R6 classes are documented manually in `@section` blocks, not
auto-generated — see [R/deferred.R](R/deferred.R) for the pattern.

## Testing notes

- Tests requiring an HTTP server use a local `webfakes` httpbin instance started
  in [tests/testthat/setup-httpbin.R](tests/testthat/setup-httpbin.R) (the `http`
  process object). Reference it as `http$url(...)` inside tests — do not hit real
  external hosts.
- `skip_without_package()` and `get_private(x)` (to reach an R6 object's private
  env) are defined in [tests/testthat/helper.R](tests/testthat/helper.R).
- Snapshot tests live in `tests/testthat/_snaps/`.

## Architecture

The package is built from three interlocking concepts. Understanding them together
is essential before changing core behaviour.

### 1. Deferred values ([R/deferred.R](R/deferred.R))

A `deferred` is an R6 object representing a value that is not yet known. It is the
basic building block for everything else. Key design points:

- **Strong ownership**: each deferred has exactly one child that receives its
  result (or error). Attaching a second child, or reusing an already-owned parent,
  is an error. `$share()` opts out of this for fan-out.
- A deferred is constructed with an `action` (run when the deferred is *started*,
  not when created) plus `parents` and `parent_resolve`/`parent_reject` callbacks
  that fire as parents resolve/reject. See the long roxygen block at the top of
  the file for the full contract, including the shorthand forms accepted for
  `parent_resolve`/`parent_reject`.
- Combinators build on this: `$then()`, `$catch()`, `$finally()`, and the
  top-level `when_all()` / `when_any()` / `when_some()`.

### 2. Synchronisation barriers ([R/synchronise.R](R/synchronise.R))

Execution alternates between a **sync phase** and **async phases**. You cannot
create/evaluate deferred values in the sync phase — you enter an async phase by
calling `synchronise(expr)`, which:

- Pushes a fresh event loop, evaluates `expr`, and if the result is a deferred,
  runs the loop until it resolves (or rejects, re-throwing the error).
- Cancels all outstanding async work on exit, interrupt, or external error.
- Enforces the barrier: a deferred chain **cannot cross** a `synchronise()`
  boundary. Loops can be nested (an inner `synchronise()` pauses the outer loop).

Lazy evaluation + auto-cancellation are consequences of this model: only deferreds
needed to compute the synchronised result are started, and ones no longer needed
are cancelled as soon as possible.

### 3. Event loop ([R/event-loop.R](R/event-loop.R))

An R6 `event_loop` class whose methods are thin wrappers delegating to free
functions named `el_*` (public) and `el__*` (private) in the same file. It
integrates several I/O sources into one poll loop:

- HTTP via `curl` multi handles (`add_http`)
- external processes and R subprocesses via `processx` (`add_process`,
  `add_r_process`)
- a worker pool of persistent R processes (`add_pool_task`, see
  [R/worker-pool.R](R/worker-pool.R))
- timers / delayed callbacks (`add_delayed`) and `add_next_tick`

When editing loop behaviour, add the method stub in the `R6Class` definition and
implement the corresponding `el_*`/`el__*` function.

### Async primitives and helpers

The async I/O constructors that produce deferreds: `delay()`
([R/delay.R](R/delay.R)), `http_get()`/`http_head()`
([R/http.R](R/http.R), [R/http-sse.R](R/http-sse.R)), `run_process()`
([R/process.R](R/process.R)), `run_r_process()` ([R/r-process.R](R/r-process.R)),
and `call_function()` (worker pool). `async()` ([R/aaa-async.R](R/aaa-async.R))
wraps a plain function into one that returns a deferred (it rewrites the function
body — see the `bquote` transformation).

Collection-style helpers built on the combinators live in files named after them:
`async_map()` ([R/map.R](R/map.R)), `async_detect()`, `async_filter()`,
`async_some()`, `async_race()`, `retry`/`backoff`, `timeout`, etc.

Dependencies (from DESCRIPTION): `callr`, `curl`, `processx`, `R6`, `utils`.
