
<!-- README.md is generated from README.Rmd. Please edit that file -->

# async

> Asynchronous HTTP

<!-- badges: start -->

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Codecov test
coverage](https://codecov.io/gh/gaborcsardi/async/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gaborcsardi/async?branch=main)
[![R-CMD-check](https://github.com/gaborcsardi/async/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gaborcsardi/async/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The async package brings asynchronous (async) computation and I/O to R.
It uses an event loop to schedule asynchronous functions that report
their results via deferred values. Deferred values can be chained
together for complex async computation, and they are evaluated lazily,
at synchronisation points.

## Features

-   A `deferred` class which is the basic building block for async
    computation.
-   Timers, HTTP queries, generic external processes, external R
    processes.
-   A worker pool for calling R functions in the background.
-   Operations to combine deferred values: `$then()`, `$when_all()`,
    `when_any()`, `when_some()`, `$finally()` and the `$catch()`
    operation to handle errors.
-   A strong ownership model: each deferred value has exactly one child
    that receives the result (or error) of its async computation.
-   Synchronisation (sync) points to embed async code into synchronous
    code. Sync points run their own event loop, and they can be stacked.
-   Lazy evaluation of deferred values. Only the deferred values that
    are needed to calculate the result of the async phase are evaluated.
-   Auto-cancellation. Deferred values that are not needed for the final
    result(s) of the async phase are automatically cancelled, as soon as
    possible.
-   Manual cancellation is also possible.
-   Progress bar support. Deferred values can report their progress via
    a callback function.
-   Additional helper functions for working with deferred values, e.g.
    `async_map()`, `async_detect()`, `async_filter()`, etc.

## Supported async I/O and computation

We support the following async primitives: - Timers: `delay()`. - HTTP
queries: `http_get()`, `http_head()`. - External processes:
`run_process()`. - External R processes: `run_r_process()`. - A workers
pool of processes to evaluate R code: `call_function()`.

## Introduction

The async package brings asychronous I/O and computation to R. It
implements asynchronous HTTP requests, timers, subprocesses and an R
worker pool.

## Deferred Values

Asynchronous computation is represented by deferred values. In async a
deferred value is an [R6](https://github.com/wch/R6) object, so it has
reference semantics. In some other programming languages deferred values
are called futures or promises.

You can think of a deferred value as a placeholder for a (non-deferred)
value that is not yet known. When the actual value of a deferred is
computed, we say that the deferred is resolved.

Typically a deferred value is created by requesting asynchronous I/O,
like an HTTP GET request with the `http_get()` function.

## Built-in Deferred Value Constructors

The async package has built-in async functions that create deferred
values: - `delay()` creates a timer that expires after the specified
time. - `http_get()` and `http_head()` perform HTTP requests,
asynchronously. - `async_constant()` creates a simple deferred that
represents the supplied value. - `run_process()` runs an external
process using processx and returns its exit code, standard output and
error, asynchronously. - `run_r_process()` runs an external R process,
and calls the specified R function in this process. It returns its exit
status, standard output, standard error, and the return value of the R
function call, asynchronously. - `call_function()` uses a worker pool of
persistent external R processes to call R functions. It returns the
return value of the function, and the standard output and error of the
process, asynchronously.

## Deferred chains

While the actual value of a deferred cannot be queried directly, we can
still operate on it, by declaring code that is to be executed, once the
value will be known:

``` r
library(async)
http_status <- function(url) {
  def <- http_get(url)$
    then(function(response) response$status_code)
}
synchronise(http_status("https://httpbin.org"))
```

    #> [1] 200

`http_status()` is an async function that returns the status code of a
GET HTTP request to the specified URL. It works by creating an async GET
request and then specifying that once the HTTP response in, the status
code should be accepted from it. The deferred value returned by
`http_status()` resolves to the status code of the URL.

`then()` returns another deferred, which also has a `then()` method, so
it is chainable.

`$then()` is the simplest combination operator on deferred values.

`$when_all()` is similar, but it creates a deferred value that resolves
once all deferred values passed to `$when_all()` are computed.

`$when_any()` creates a deferred value that resolves as soon as one of
its arguments successfully resolves. `when_some()` is its generalization
that requires the computation of a given number of deferred values.

## Synchronization barriers

async allows embedding asynchronous computation in synchronous code. The
execution of such a program has a sync phase and async phases. When the
program starts, it is in the sync phase. In the sync phase you cannot
create deferred values. (But you can still define (async) functions,
that will create deferred values when called.)

To enter into an async phase, call `synchronise()` on an expression that
evaluates to a deferred value. The async phase will last until this
deferred value is computed or an error is thrown (and the error reaches
`synchronise()`).

`synchronise()` creates an event loop, which manages the computation of
the deferred values in this particular async phase.

Async phases can be embedded into each other. I.e. a program may call
`synchronise()` while in the async phase. The outer async phase’s event
loop then stops until the inner async phase terminates. Deferred values
cannot be passed through a `synchronise()` barrier, to anoter (sync or
async phase). Should this happen, an error is reported on the first
operation on the leaked deferred value.

In a typical application, a function is implemented asynchronously, and
then used synchronously by the interactive user, or another piece of
synchronous code, via `synchronise()` calls. The following example makes
three HTTP requests in parallel:

``` r
http_status3 <- function() {
  http_status <- function(url) {
    http_get(url)$then(function(response) response$status_code)
  }
  r1 <- http_status("https://httpbin.org/status/403")
  r2 <- http_status("https://httpbin.org/status/404")
  r3 <- http_status("https://httpbin.org/status/200")
  when_all(r1, r2, r3)
}
synchronise(http_status3())
```

    #> [[1]]
    #> [1] 403
    #> 
    #> [[2]]
    #> [1] 404
    #> 
    #> [[3]]
    #> [1] 200

## Error handling

There are two ways to handle errors in asynchronous code. One is the
`$catch()` operation that can be called on a deferred value. `$catch()`
has similar syntax as `tryCatch()`. It can be used to catch errors
during the computation of the deferred value, including computation in
its ancestors (except for the errors the ancestors already handle).

``` r
response_time <- async(function(url) {
  http_head(url)$
    then(http_stop_for_status)$
    then(function(x) setNames(x[["times"]][["total"]], url))$
    catch(error = function(...) setNames(Inf, url))
})
synchronise(response_time("https://google.com"))
```

    #> https://google.com 
    #>           0.213394

``` r
synchronise(response_time("https://httpbin.org/status/401"))
```

    #> https://httpbin.org/status/401 
    #>                            Inf

Errors can also be handled synchronously. If an error is not handled
asynchronously, then the deferred value will throw an error when itself
or one of its descendants is `synchronise()`-ed. This can be caught with
`tryCatch()`.

## Ownership and the Async DAG

When the `$then()` method of a deferred value is called to create
another deferred value:

``` r
d2 <- d1$then(function(x) ...)
```

then we say that `d2` owns `d1`. We also say that `d2` is the child of
`d1`, and `d1` is the parent of `d2`. async has a strong ownership
model, and it only allows a single owner (i.e. a single child) for each
deferred. The parent-child relationships define a directed forest graph,
a collection of directed trees. (This is without shared deferred values,
see the manual.)

The strong ownership model does not allow calling `$then()` multiple
times on the same deferred value, i.e. the following generates an error:

``` r
do <- function() {
  d <- delay(1/100)
  d$then(function() print("foo"))
  d$then(function() print("bar"))
}
synchronise(do())
```

    #> Error in def__add_as_parent(self, private, child): Deferred value is already owned

The `when_all()`, `when_any()` and `when_some()` operations set a single
deferred as the owner of multiple parents. For `when_all()` the child
node is resolved once all of its parents are resolved (or one throws an
error). `when_any()` resolves as soon as one of its parents resolves. If
all of its parents throw errors then `when_any()` throws as well.
`when_some()` is a generalization of `when_any()` and it resolves as
soon as the specified number of its parents resolve without error, or if
too many parents fail for `when_some()` to be successful.

When `synchronise()` is called on a deferred value, the DAG rooted there
is called the async DAG of the async phase. (This is usually a directed
tree, and in this README we do not deal with shared deferred values,
which would result more general DAGs.)

When the strict shared ownership model is too restrictive, certain
deferred values can be marked as shared, via the `$share()` method.
These can have multiple owners (children) and they are also not
auto-cancelled (see Auto-Cancellation later).

## Lazy Evaluation

async does not evaluate deferred values that are not part of the async
DAG of the async phase. These are clearly not needed to compute the
result of the async phase, so it would be a waste of resources working
on them. (It is also unclear how their errors should be handled.)

In the following example, `d1` and `d2` are created, but they are not
part of the async DAG, so they are never evaluated.

``` r
do <- function() {
  d1 <- delay(1/100)$then(function() print("d1"))
  d2 <- d1$then(function() print("d2"))
  d3 <- delay(1/100)$then(function() print("d3"))
  d4 <- d3$then(function() print("d4"))
  d4
}
invisible(synchronise(do()))
```

    #> [1] "d3"
    #> [1] "d4"

## Auto-Cancellation

In an async phase, it might happen that parts of the async DAG are not
needed for the final result any more. E.g. if a parent of a `when_all()`
node throws an error, then the other parents don’t have to be computed.
In this case the event loop of the phase automatically cancels these
deferred values. Similarly, if a single parent of a `when_any()` node is
resolved, the other parents can be cancelled.

In general, if a node of the async DAG is resolved, the whole directed
DAG, rooted at that node, can be cancelled (except for nodes that were
already resolved and nodes that have already failed).

Auto-cancellation is very convenient, as you can be sure that resources
are free as soon as they are not needed. Some practical examples:

-   Making HTTP requests to many mirror web sites, to check their
    response time. As soon as the first reply is in, the rest of the
    HTTP requests are cancelled.
-   In multi-process computation, as soon as one process fails, the rest
    are automatically cancelled. (Unless the failure is handled, of
    course.)

async also has another type of cancellation, when `synchronise()` is
interrupted externally, either by the user or some system error. In this
case all processes and resources that were created in the event loop,
are cancelled and freed.

Shared deferred values (see `$share()`) are not auto-cancelled when
their children are resolved or errored, but they are always cancelled at
the end of the async phase.

## Async Iterators

async provides some utilities that make it easier to deal with
collections of deferred values. E.g. `async_map()` applies an async
function to a list and returns a single deferred value for the whole
result. `async_detect()` finds a value in a list that satisfies an async
predicate function, etc.

The current iterators: \* `async_map()` applies an async function to all
elements of a vector or list (collection). \* `async_detect()` finds an
element of a collection that passed an async truth test. \*
`async_every()` checks if every element of a collection satisfies an
async predicate. `async_some()` checks if any element does that. \*
`async_filter()` keeps elements that pass an async truth test.

## Async Control Flow

Control flow with deferred values can be challenging. Some helpers: \*
`async_reflect()` creates an async function that always succeeds. This
is useful if you want to apply it to a collection, and don’t want to
stop at the first error. \* `async_retry()` tries an async function a
number of times. `async_retryable()` turns a regular function into a
retryable one. \* `async_sequence()` chains two async functions. Calling
their sequence is equivalent calling `then()` on them, but
`async_sequence()` is easier to use programmatically. \* `async_until()`
and `async_whilst()` let you call an async function repeatedly, until or
while a (syncronous) condition holds. \* `async_timeout()` runs an async
function with a timeout.

## Other Async Utilities

-   `async_constant()` takes a value and creates and asynchronous
    function that returns that value.

## Examples

### Parallel API Queries

Query the crandb API, get the authors of the packages with the most
reverse dependencies.

``` r
fromJSON <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)
revdep_authors <- function() {
  get_author <- function(package) {
    url <- paste0("https://crandb.r-pkg.org/", package)
    http_get(url)$
      then(function(x) fromJSON(rawToChar(x$content)))$
      then(function(x) x$Author)
  }

  http_get("https://crandb.r-pkg.org/-/topdeps/devel")$
    then(function(x) fromJSON(rawToChar(x$content)))$
    then(function(x) names(unlist(x)))$
    then(function(x) async_map(x, get_author))
}
synchronise(revdep_authors())[1:3]
```

    #> [[1]]
    #> [1] "Yihui Xie [aut, cre] (<https://orcid.org/0000-0003-0645-5666>),\nAbhraneel Sarma [ctb],\nAdam Vogt [ctb],\nAlastair Andrew [ctb],\nAlex Zvoleff [ctb],\nAndre Simon [ctb] (the CSS files under inst/themes/ were derived from\nthe Highlight package http://www.andre-simon.de),\nAron Atkins [ctb],\nAaron Wolen [ctb],\nAshley Manton [ctb],\nAtsushi Yasumoto [ctb] (<https://orcid.org/0000-0002-8335-495X>),\nBen Baumer [ctb],\nBrian Diggs [ctb],\nBrian Zhang [ctb],\nBulat Yapparov [ctb],\nCassio Pereira [ctb],\nChristophe Dervieux [ctb],\nDavid Hall [ctb],\nDavid Hugh-Jones [ctb],\nDavid Robinson [ctb],\nDoug Hemken [ctb],\nDuncan Murdoch [ctb],\nElio Campitelli [ctb],\nEllis Hughes [ctb],\nEmily Riederer [ctb],\nFabian Hirschmann [ctb],\nFitch Simeon [ctb],\nForest Fang [ctb],\nFrank E Harrell Jr [ctb] (the Sweavel package at inst/misc/Sweavel.sty),\nGarrick Aden-Buie [ctb],\nGregoire Detrez [ctb],\nHadley Wickham [ctb],\nHao Zhu [ctb],\nHeewon Jeon [ctb],\nHenrik Bengtsson [ctb],\nHiroaki Yutani [ctb],\nIan Lyttle [ctb],\nHodges Daniel [ctb],\nJake Burkhead [ctb],\nJames Manton [ctb],\nJared Lander [ctb],\nJason Punyon [ctb],\nJavier Luraschi [ctb],\nJeff Arnold [ctb],\nJenny Bryan [ctb],\nJeremy Ashkenas [ctb, cph] (the CSS file at\ninst/misc/docco-classic.css),\nJeremy Stephens [ctb],\nJim Hester [ctb],\nJoe Cheng [ctb],\nJohannes Ranke [ctb],\nJohn Honaker [ctb],\nJohn Muschelli [ctb],\nJonathan Keane [ctb],\nJJ Allaire [ctb],\nJohan Toloe [ctb],\nJonathan Sidi [ctb],\nJoseph Larmarange [ctb],\nJulien Barnier [ctb],\nKaiyin Zhong [ctb],\nKamil Slowikowski [ctb],\nKarl Forner [ctb],\nKevin K. Smith [ctb],\nKirill Mueller [ctb],\nKohske Takahashi [ctb],\nLorenz Walthert [ctb],\nLucas Gallindo [ctb],\nMarius Hofert [ctb],\nMartin Modrák [ctb],\nMichael Chirico [ctb],\nMichael Friendly [ctb],\nMichal Bojanowski [ctb],\nMichel Kuhlmann [ctb],\nMiller Patrick [ctb],\nNacho Caballero [ctb],\nNick Salkowski [ctb],\nNiels Richard Hansen [ctb],\nNoam Ross [ctb],\nObada Mahdi [ctb],\nPavel N. Krivitsky [ctb] (<https://orcid.org/0000-0002-9101-3362>),\nQiang Li [ctb],\nRamnath Vaidyanathan [ctb],\nRichard Cotton [ctb],\nRobert Krzyzanowski [ctb],\nRomain Francois [ctb],\nRuaridh Williamson [ctb],\nScott Kostyshak [ctb],\nSebastian Meyer [ctb],\nSietse Brouwer [ctb],\nSimon de Bernard [ctb],\nSylvain Rousseau [ctb],\nTaiyun Wei [ctb],\nThibaut Assus [ctb],\nThibaut Lamadon [ctb],\nThomas Leeper [ctb],\nTim Mastny [ctb],\nTom Torsney-Weir [ctb],\nTrevor Davis [ctb],\nViktoras Veitas [ctb],\nWeicheng Zhu [ctb],\nWush Wu [ctb],\nZachary Foster [ctb],\nZhian N. Kamvar [ctb] (<https://orcid.org/0000-0003-1458-7108>)"
    #> 
    #> [[2]]
    #> [1] "Hadley Wickham [aut, cre],\nRStudio [cph, fnd],\nR Core team [ctb] (Implementation of utils::recover())"
    #> 
    #> [[3]]
    #> [1] "JJ Allaire [aut],\nYihui Xie [aut, cre] (<https://orcid.org/0000-0003-0645-5666>),\nJonathan McPherson [aut],\nJavier Luraschi [aut],\nKevin Ushey [aut],\nAron Atkins [aut],\nHadley Wickham [aut],\nJoe Cheng [aut],\nWinston Chang [aut],\nRichard Iannone [aut] (<https://orcid.org/0000-0003-3925-190X>),\nAndrew Dunning [ctb] (<https://orcid.org/0000-0003-0464-5036>),\nAtsushi Yasumoto [ctb, cph] (<https://orcid.org/0000-0002-8335-495X>,\nNumber sections Lua filter),\nBarret Schloerke [ctb],\nCarson Sievert [ctb] (<https://orcid.org/0000-0002-4958-2844>),\nChristophe Dervieux [ctb],\nDevon Ryan [ctb] (<https://orcid.org/0000-0002-8549-0971>),\nFrederik Aust [ctb] (<https://orcid.org/0000-0003-4900-788X>),\nJeff Allen [ctb],\nJooYoung Seo [ctb] (<https://orcid.org/0000-0002-4064-6012>),\nMalcolm Barrett [ctb],\nRob Hyndman [ctb],\nRomain Lesur [ctb],\nRoy Storey [ctb],\nRuben Arslan [ctb],\nSergio Oller [ctb],\nRStudio, PBC [cph],\njQuery UI contributors [ctb, cph] (jQuery UI library; authors listed in\ninst/rmd/h/jqueryui-AUTHORS.txt),\nMark Otto [ctb] (Bootstrap library),\nJacob Thornton [ctb] (Bootstrap library),\nBootstrap contributors [ctb] (Bootstrap library),\nTwitter, Inc [cph] (Bootstrap library),\nAlexander Farkas [ctb, cph] (html5shiv library),\nScott Jehl [ctb, cph] (Respond.js library),\nIvan Sagalaev [ctb, cph] (highlight.js library),\nGreg Franko [ctb, cph] (tocify library),\nJohn MacFarlane [ctb, cph] (Pandoc templates),\nGoogle, Inc. [ctb, cph] (ioslides library),\nDave Raggett [ctb] (slidy library),\nW3C [cph] (slidy library),\nDave Gandy [ctb, cph] (Font-Awesome),\nBen Sperry [ctb] (Ionicons),\nDrifty [cph] (Ionicons),\nAidan Lister [ctb, cph] (jQuery StickyTabs),\nBenct Philip Jonsson [ctb, cph] (pagebreak Lua filter),\nAlbert Krewinkel [ctb, cph] (pagebreak Lua filter)"

### Checking URLs

The following code returns the 2 URLs that respond with the shortest
response time.

``` r
response_time <- async(function(url) {
  http_head(url)$
    then(http_stop_for_status)$
    then(function(x) setNames(x[["times"]][["total"]], url))$
    catch(error = function() setNames(Inf, url))
})

fastest_urls <- async(function(urls, n = 2) {
  reqs <- lapply(urls, response_time)
  when_some(n, .list = reqs)$
    then(function(x) sort(unlist(x)))
})

urls <- c("https://cran.rstudio.com", "https://cran.r-project.org",
          "https://www.stats.bris.ac.uk/R/", "https://cran.uib.no/")

synchronise(fastest_urls(urls))
```

    #>   https://cran.rstudio.com https://cran.r-project.org 
    #>                   0.057224                   0.174976

See the package vignettes for more examples.

## License

MIT © [RStudio Inc](https://rstudio.com)
