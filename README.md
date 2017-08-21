


# async

> Asynchronous HTTP

[![Linux Build Status](https://travis-ci.org/rstudio/async.svg?branch=master)](https://travis-ci.org/rstudio/async)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/rstudio/async?svg=true)](https://ci.appveyor.com/project/gaborcsardi/async)
[![](http://www.r-pkg.org/badges/version/async)](http://www.r-pkg.org/pkg/async)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/async)](http://www.r-pkg.org/pkg/async)

The `async` package brings asynchronous (async) computation and I/O to R.
It uses an event loop to schedule asynchronous functions (tasks) that
report their results with a delay, via callbacks.

## Installation


```r
devtools::install_github("rstudio/async")
```

## Introduction

`async` also provides
* async flow control constructs, e.g. to call a sequence of tasks
  (`waterfall`), or to iterate an async function until a condition holds
  (`until`).
* async iterators, that perform an operation on all elements of a vector
  or list or similar structure. E.g. `amap`, `each`, `detect`, etc.
* async utilities, to convert a synchronous function to async, or the other
  way, etc. See e.g. `asyncify` and `syncify`.

Each task has an id, which is a randomly generated character string. This
id can be used to wait for the task to complete, synchronously, via the
`wait_for` function.

Here is an example. `check_url` checks if we can make a successful HTTP
HEAD request to a URL. It is an async function, it reports the result via
a callback. It uses `http_get` to perform the HTTP request:


```r
library(async)
check_url <- function(url, callback) {
  http_head(url, function(err, res) {
    if (is.null(err) && res$status_code < 300) {
      callback(NULL, TRUE)
    } else {
	  callback(NULL, FALSE)
	}
  })
}
```

To perform a request and wait for the result, we can use


```r
result <- NULL
id <- check_url("https://httpbin.org", function(err, res) {
  if (!is.null(err)) stop(err)
  result <<- res
})
wait_for(id)
result
```

```
#> [1] TRUE
```

We can use `check_url` in an async iterator, to check multiple URLs.
`amap` performs a task on each element of a vector or list, and reports
the results in a list:


```r
result <- NULL
id <- amap(
  c("https://httpbin.org", "https://httpbin.org/status/404"),
  check_url,
  function(err, res) {
    if (!is.null(err)) stop(err)
    result <<- res
  }
)
wait_for(id)
result
```

```
#> [[1]]
#> [1] TRUE
#> 
#> [[2]]
#> [1] FALSE
```

`amap` performs `check_url` on both items in the list, in parallel.
`wait_for` waits until `amap` is complete, i.e. until both `check_url`
calls are done.

## Async Iterators

TODO

## Async Flow Control

TODO

## Async Utilities

TODO

## License

MIT © [RStudio Inc](https://rstudio.com)
