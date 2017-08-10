
# async

> Asynchronous HTTP

[![Linux Build Status](https://travis-ci.org/gaborcsardi/async.svg?branch=master)](https://travis-ci.org/gaborcsardi/async)

[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/async?svg=true)](https://ci.appveyor.com/project/gaborcsardi/async)
[![](http://www.r-pkg.org/badges/version/async)](http://www.r-pkg.org/pkg/async)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/async)](http://www.r-pkg.org/pkg/async)


Define tasks, that can stop on an HTTP request, and then continue later, once
  the HTTP response is in. An event loop keeps the tasks running.

## Installation

```r
devtools::install_github("gaborcsardi/async")
```

## Usage

```r
library(async)
```

## License

MIT + file LICENSE © 
