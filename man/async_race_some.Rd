% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/race.R
\name{async_race_some}
\alias{async_race_some}
\alias{async_race}
\title{A deferred value that resolves when the specified number of deferred
values resolve, or is rejected when one of them is rejected}
\usage{
async_race_some(count, ..., .list = list())

async_race(..., .list = list())
}
\arguments{
\item{count}{Number of deferred values that need to resolve.}

\item{...}{Deferred values.}

\item{.list}{More deferred values.}
}
\value{
A deferred value, that is conditioned on all deferred values
in \code{...} and \code{.list}.
}
\description{
These functions are similar to \code{\link[=when_some]{when_some()}} and \code{\link[=when_any]{when_any()}}, but they
do not ignore errors. If a deferred is rejected, then \code{async_race_some()} and
\code{async_race()} are rejected as well.
}
\details{
\code{async_race()} is a special case of \verb{count = }: it resolves or is rejected
as soon as one deferred resolves or is rejected.

async has auto-cancellation, so if the required number of deferred values
are resolved, or any deferred value is rejected, the rest are cancelled.
}
