
#' Asynchronous HTTP
#'
#' @section Introduction:
#' 
#' The `async` package brings asynchronous computation and I/O to R.
#'
#' It has an event loop that takes care of scheduling multiple asynchronous
#' functions (tasks), and polling for input from HTTP and other sources.
#'
#' @section Async Functions:
#'
#' Asynchronous computation happens in special functions called tasks.
#' A task is a function that reports its completion asynchronously, via
#' calling a callback function.
#'
#' `async` provides some async functions to perform HTTP requests
#' (`http_get`, `http_head`, etc.), and code execution with a delay
#' (`set_timeout`).
#'
#' @section Async Iterators:
#' 
#'
#' @section Async Flow Control:
#'
#' @section List iterators:
#'
#' The package also provides several utilities to make it easier to
#' program with tasks and avoid deeply nested callbacks.
#'
#' TODO: more
#'
#' @section Synchronization:
#'
#' TODO
#'
#' @docType package
#' @name async
NULL
