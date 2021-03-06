% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yrq funs.R
\name{yrqfuns}
\alias{yrqfuns}
\alias{get.q}
\alias{get.y}
\alias{next.yrq}
\alias{last.yrq}
\alias{next.fall}
\title{Next/last academic year-quarters, i.e. yyyyq format.}
\usage{
get.q(x)

get.y(x)

next.yrq(x)

last.yrq(x)

next.fall(x)
}
\arguments{
\item{x}{a five digit academic year-quarter (yrq) code, e.g. 20014 for Fall 2001.}
}
\value{
An integer - either a subset fo the year-quarter or a modified 5-digit code.
}
\description{
Basic functions to deal with year-quarter codes.
  The functions get.q() and get.y() return a single quarter or year.
  The 'next' functions return the next year-quarter or next fall.
}
\examples{
get.q(19804)
get.y(19804)

x <- c(20013, 19752, 20183)
get.q(x)
get.y(x)
next.yrq(x)
next.fall(x)
}
