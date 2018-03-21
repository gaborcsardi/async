


# async

> Asynchronous HTTP

[![Linux Build Status](https://travis-ci.org/r-lib/async.svg?branch=master)](https://travis-ci.org/r-lib/async)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/async?svg=true)](https://ci.appveyor.com/project/gaborcsardi/async)
[![](http://www.r-pkg.org/badges/version/async)](http://www.r-pkg.org/pkg/async)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/async)](http://www.r-pkg.org/pkg/async)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/async/master.svg)](https://codecov.io/github/r-lib/async?branch=master)

The `async` package brings asynchronous (async) computation and I/O to R.
It uses an event loop to schedule asynchronous functions that
report their results via deferred values.

## Installation


```r
devtools::install_github("r-lib/async")
```

## Introduction

The `async` package brings asychronous I/O and computation to R. As a first
step it implements asynchronous HTTP requests.

## Async functions

Asynchronous computation is carried out in asynchronous (async) functions.
An async function is special:
- it can stop its execution to wait for results from other async
  functions.
- it can create deferred values, that represent asynchronous computation.
- it returns a deferred value.
- it can call other asynchronous functions.

The `async` package contains some async functions:
- `delay()` invokes computation that is carried out after a timer expires.
- `http_get()` and `http_head()` perform HTTP requests, asynchronously.
- `async_constant()` is an async function that represents a value.

More async functions can be created using `async()`.

## Synchronization barriers

Asynchronous computation is carried out in async functions, and async
functions can call other async functions. Usually synchronous functions
cannot call asynchronous functions, except for a special function:
`synchronise()`. `synchronise()` creates a synchronization barrier. It
can call an async function, and it waits until the async function is run
to completion. `synchronise()` is a tool that allows embedding
asynchronous code into synchronous code. All examples in this intro are
embedded into a `synchronise()` call.

## Deferred Values

Asynchronous computation is represented by deferred values. In `async`
a deferred value is an [R6](https://github.com/wch/R6) object, so it has
reference semantics. In some other programming languages deferred values
are called futures or promises.

You can think of a deferred value as a placeholder for a (non-deferred)
value that is not yet known. A deferred value can be in three states:
* pending
* fulfilled
* rejected

It starts up in the pending state, and once it is fulfilled or rejected,
it cannot change.

Typically a deferred value is created by requesting asynchronous I/O, like
an HTTP GET request with the `http_get()` function.

## Deferred chains

While the eventual value of a pending deferred is not known, we can still
operate on it, by declaring code that is to be executed, once the value
will be known.


```r
library(async)
afun <- async(function() {
  def <- http_get("https://httpbin.org")
  status <- def$then(function(response) response$status_code)
  print(status)
})
synchronise(afun())
```

```
#> <deferred>
#>   Public:
#>     cancel: function (reason = NULL) 
#>     catch: function (on_rejected) 
#>     clone: function (deep = FALSE) 
#>     finally: function (on_finally) 
#>     get_event_loop: function () 
#>     get_value: function () 
#>     initialize: function (action, on_progress = NULL, on_cancel = NULL, parent = NULL) 
#>     then: function (on_fulfilled = NULL, on_rejected = NULL) 
#>   Private:
#>     cancel_callback: NULL
#>     cancelled: FALSE
#>     event_loop: event_loop, R6
#>     make_error_object: function (err) 
#>     on_fulfilled: list
#>     on_rejected: list
#>     parent: deferred, R6
#>     progress: function (data) 
#>     progress_callback: NULL
#>     reject: function (reason) 
#>     resolve: function (value) 
#>     start_stack: NULL
#>     state: pending
#>     value: NULL
```

```
#> [1] "pending"
```

`then()` returns another deferred, which also has a `then()` method,
so it is chainable. It is possible to split the chain, and call the
`then()` method on the same deferred multiple times, to schedule multiple
operations on the resolved value.

## Synchronization

The `synchroniset()` function allows mixing synchronous and asynchronous
code. It can be called on an expression that creates deferred values,
and it stops the synchronous until all deferred values are resolved
(i.e. fulfilled or rejected). Importantly, `synchronise()` creates a new
event loop and works towards he resolution of all deferred values
created within this event loop.

In a typical application, a function is implemented asynchronously, and
then used synchronously by the interactive user, or another piece of
synchronous code, via `synchronise()` calls. The following example makes
three HTTP requests in parallel:


```r
afun <- async(function() {
  http_status <- function(url) {
    http_get(url)$then(function(response) response$status_code)
  }
  r1 <- http_status("https://httpbin.org/status/403")
  r2 <- http_status("https://httpbin.org/status/404")
  r3 <- http_status("https://httpbin.org/status/200")
  when_all(r1, r2, r3)
})
synchronise(afun())
```

```
#> [[1]]
#> [1] 403
#> 
#> [[2]]
#> [1] 404
#> 
#> [[3]]
#> [1] 200
```

## Error handling

There are two ways to handle errors in asynchronous code.
`then()` can take a second argument, another function, that is called
if and when the deferred value is rejected. This function catches errors
and by default turns them into regular values. Alternatively, they can
also re-throw the error by calling `stop()`.


```r
afun <- async(function() {
  u1 <- http_get("https://httpbin.org")$
    then(function() "web server is up", function() "web server is down")
  u2 <- http_get("non-existing-url.for-sure")$
    then(function() "web server is up", function() "web server is down")
  when_all(u1, u2)
})
synchronise(afun())
```

```
#> [[1]]
#> [1] "web server is up"
#> 
#> [[2]]
#> [1] "web server is down"
```

The `catch()` method is handy to write an error handler only, it is
equivalent to `then()` with its first argument set to `NULL`.

Errors can also be handled synchronously. If an error is not handled
asynchronously in a `then()` or `catch()` method, then the deferred value
will throw an error when it is `synchronise()`-ed. This can be caught via
`tryCatch()`.

## Async Iterators

`async` provides some utilities that make it easier to deal with
collections of deferred values. E.g. `async_map()` applies an async function
to a list and returns a single deferred value for the whole result.
`async_detect()` finds a value in a list that satisfies an async predicate
function, etc.

The current iterators:
* `async_map()` applies an async function to all elements of a vector or
  list (collection).
* `async_detect()` finds an element of a collection that passed an async
  truth test.
* `async_every()` checks if every element of a collection satisfies an async
  predicate. `async_some()` checks if any element does that.
* `async_filter()` keeps elements that pass an async truth test.
* `async_timeout()` runs an async function with a timeout.

## Async Control Flow

Control flow with deferred values can be challenging. Some helpers:
* `async_reflect()` creates an async function that always succeeds.
  This is useful if you want to apply it to a collection, and don't
  want to stop at the first error.
* `async_retry()` tries an async function a number of times.
  `async_retryable()` turns a regular function into a retryable one.
* `async_sequence()` chains two async functions. Calling their sequence is
  equivalent calling `then()` on them, but `async_sequence()` is easier to
  use programmatically.
* `async_until()` and `async_whilst()` let you call an async function
  repeatedly, until or while a (syncronous or asynchronous) condition
  holds.

## Other Async Utilities

* `async()` converts a synchronous function to async.
* `async_constant()` takes a value and creates and asynchronous function
  that returns that value.
* `when_all()` returns a deferred value that is resolved when all supplied
  deferred values are resolved. `when_any()` and `when_some()` are similar,
  but condition on a single, or the specified number of deferred values.

## Examples

### Parallel API Queries

Query the crandb API, get the authors of the packages with the most
reverse dependencies.


```r
fromJSON <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)
revdep_authors <- async(function() {
  get_author <- function(package) {
    url <- paste0("https://crandb.r-pkg.org/", package)
    http_get(url)$
      then(~ fromJSON(rawToChar(.$content)))$
      then(~ .$Author)
  }

  http_get("https://crandb.r-pkg.org/-/topdeps/devel")$
    then(~ fromJSON(rawToChar(.$content)))$
    then(~ names(unlist(.)))$
    then(~ async_map(., get_author))
})
synchronise(revdep_authors())[1:3]
```

```
#> [[1]]
#> [1] "Yihui Xie [aut, cre] (<https://orcid.org/0000-0003-0645-5666>),\nAdam Vogt [ctb],\nAlastair Andrew [ctb],\nAlex Zvoleff [ctb],\nAndre Simon [ctb] (the CSS files under inst/themes/ were derived from\nthe Highlight package http://www.andre-simon.de),\nAron Atkins [ctb],\nAaron Wolen [ctb],\nAshley Manton [ctb],\nBen Baumer [ctb],\nBrian Diggs [ctb],\nCassio Pereira [ctb],\nChristophe Dervieux [ctb],\nDavid Hugh-Jones [ctb],\nDavid Robinson [ctb],\nDonald Arseneau [ctb, cph] (the framed package at inst/misc/framed.sty),\nDoug Hemken [ctb],\nDuncan Murdoch [ctb],\nElio Campitelli [ctb],\nFabian Hirschmann [ctb],\nFitch Simeon [ctb],\nForest Fang [ctb],\nFrank E Harrell Jr [ctb] (the Sweavel package at inst/misc/Sweavel.sty),\nGarrick Aden-Buie [ctb],\nGregoire Detrez [ctb],\nHadley Wickham [ctb],\nHeewon Jeon [ctb],\nHenrik Bengtsson [ctb],\nHiroaki Yutani [ctb],\nIan Lyttle [ctb],\nHodges Daniel [ctb],\nJake Burkhead [ctb],\nJames Manton [ctb],\nJared Lander [ctb],\nJason Punyon [ctb],\nJavier Luraschi [ctb],\nJeff Arnold [ctb],\nJenny Bryan [ctb],\nJeremy Ashkenas [ctb, cph] (the CSS file at\ninst/misc/docco-classic.css),\nJeremy Stephens [ctb],\nJim Hester [ctb],\nJoe Cheng [ctb],\nJohannes Ranke [ctb],\nJohn Honaker [ctb],\nJohn Muschelli [ctb],\nJonathan Keane [ctb],\nJJ Allaire [ctb],\nJohan Toloe [ctb],\nJonathan Sidi [ctb],\nJoseph Larmarange [ctb],\nJulien Barnier [ctb],\nKaiyin Zhong [ctb],\nKamil Slowikowski [ctb],\nKarl Forner [ctb],\nKevin K. Smith [ctb],\nKirill Mueller [ctb],\nKohske Takahashi [ctb],\nMartin Modrák [ctb],\nMichael Chirico [ctb],\nMichael Friendly [ctb],\nMichal Bojanowski [ctb],\nMichel Kuhlmann [ctb],\nNacho Caballero [ctb],\nNick Salkowski [ctb],\nNoam Ross [ctb],\nObada Mahdi [ctb],\nQiang Li [ctb],\nRamnath Vaidyanathan [ctb],\nRichard Cotton [ctb],\nRobert Krzyzanowski [ctb],\nRomain Francois [ctb],\nRuaridh Williamson [ctb],\nScott Kostyshak [ctb],\nSebastian Meyer [ctb],\nSietse Brouwer [ctb],\nSimon de Bernard [ctb],\nSylvain Rousseau [ctb],\nTaiyun Wei [ctb],\nThibaut Assus [ctb],\nThibaut Lamadon [ctb],\nThomas Leeper [ctb],\nTom Torsney-Weir [ctb],\nTrevor Davis [ctb],\nViktoras Veitas [ctb],\nWeicheng Zhu [ctb],\nWush Wu [ctb],\nZachary Foster [ctb]"
#> 
#> [[2]]
#> [1] "Hadley Wickham [aut, cre],\nRStudio [cph, fnd],\nR Core team [ctb] (Implementation of utils::recover())"
#> 
#> [[3]]
#> [1] "JJ Allaire [aut],\nYihui Xie [aut, cre] (<https://orcid.org/0000-0003-0645-5666>),\nJonathan McPherson [aut],\nJavier Luraschi [aut],\nKevin Ushey [aut],\nAron Atkins [aut],\nHadley Wickham [aut],\nJoe Cheng [aut],\nWinston Chang [aut],\nJeff Allen [ctb],\nRoy Storey [ctb],\nRob Hyndman [ctb],\nRuben Arslan [ctb],\nRStudio, Inc. [cph],\njQuery Foundation [cph] (jQuery library),\njQuery contributors [ctb, cph] (jQuery library; authors listed in\ninst/rmd/h/jquery-AUTHORS.txt),\njQuery UI contributors [ctb, cph] (jQuery UI library; authors listed in\ninst/rmd/h/jqueryui-AUTHORS.txt),\nMark Otto [ctb] (Bootstrap library),\nJacob Thornton [ctb] (Bootstrap library),\nBootstrap contributors [ctb] (Bootstrap library),\nTwitter, Inc [cph] (Bootstrap library),\nAlexander Farkas [ctb, cph] (html5shiv library),\nScott Jehl [ctb, cph] (Respond.js library),\nIvan Sagalaev [ctb, cph] (highlight.js library),\nGreg Franko [ctb, cph] (tocify library),\nJohn MacFarlane [ctb, cph] (Pandoc templates),\nGoogle, Inc. [ctb, cph] (ioslides library),\nDave Raggett [ctb] (slidy library),\nW3C [cph] (slidy library),\nDave Gandy [ctb, cph] (Font-Awesome),\nBen Sperry [ctb] (Ionicons),\nDrifty [cph] (Ionicons),\nAidan Lister [ctb, cph] (jQuery StickyTabs)"
```

### Checking URLs

The following code returns the 2 URLs that respond first.


```r
fastest_two <- async(function(urls) {
  qs <- lapply(urls, http_head)
  t2 <- when_some(2, .list = qs)$
  then(function(top2) vapply(top2, "[[", character(1), "url"))
})
urls <- c("https://cran.rstudio.com", "https://cran.r-project.org",
          "https://www.stats.bris.ac.uk/R/", "https://cran.uib.no/")
synchronise(
  fastest_two(urls)
)
```

```
#> [1] "https://cran.rstudio.com/"       "https://www.stats.bris.ac.uk/R/"
```

## License

MIT © [RStudio Inc](https://rstudio.com)
