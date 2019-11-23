# Functions for quarters only ---------------------------------------------

#' Next/last academic quarters
#'
#' @description Simple functions to correctly increment academic quarters. I.e. 4 (Fall) should roll to 1 (Winter).
#' @param x a quarter, numeric from 1:4 (Winter > Spring > Summer > Fall)
#' @return The next, or prior, academic quarter, an integer from 1:4.
#' @examples
#' next.qtr(4)
#' x <- c(1, 3, 4, 2, 4)
#' next.qtr(x)
#' last.qtr(x)
#' @name qtrfuns
NULL


#' Next quarter
#' @rdname qtrfuns
#' @export
next.qtr <- function(x){
  ifelse(x == 4, 1, x + 1)
}

#' Previous quarter
#' @rdname qtrfuns
#' @export
last.qtr <- function(x){
  ifelse(x == 1, 4, x - 1)
}

# Functions for yrqs (yyyyq) ----------------------------------------------
#' Next/last academic year-quarters, i.e. yyyyq format.
#'
#' @description Basic functions to deal with year-quarter codes.
#'   The functions get.q() and get.y() return a single quarter or year.
#'   The 'next' functions return the next year-quarter or next fall.
#' @param x a five digit academic year-quarter (yrq) code, e.g. 20014 for Fall 2001.
#' @return An integer - either a subset fo the year-quarter or a modified 5-digit code.
#' @examples
#' get.q(19804)
#' get.y(19804)
#'
#' x <- c(20013, 19752, 20183)
#' get.q(x)
#' get.y(x)
#' next.yrq(x)
#' next.fall(x)
#' @name yrqfuns
NULL

#' Return the quarter from yyyyq
#' @rdname yrqfuns
#' @export
get.q <- function(x) { x %% 10 }

#' Return year from yyyyq
#' @rdname yrqfuns
#' @export
get.y <- function(x) { x %/% 10 }

#' Get next yyyyq
#' @rdname yrqfuns
#' @export
next.yrq <- function(x){
  ifelse((x %% 10) == 4, x + 7, x + 1)
}

#' Get last yyyyq
#' @rdname yrqfuns
#' @export
last.yrq <- function(x){
  ifelse((x %% 10) == 1, x - 7, x - 1)
}

#' Get next fall yyyyq
#' @rdname yrqfuns
#' @export
next.fall <- function(x){
  ifelse(get.q(x) < 4,
         get.y(x) * 10 + 4,
         x+10)
}


# Functions for 2 yrq inputs ----------------------------------------------

#' Difference two year-quarters
#'
#' @param yrq1,yrq2 two 5-digit year-quarters.
#' @return The difference between two academic year-quarters, expressed in quarters.
#'   Does not return absolute value, so if yrq1 < yrq2, result will be negative.
#' @examples
#' qtr.diff(20054, 20044)
#' qtr.diff(19994, 20001)
#'
#' x <- c(20031, 19804, 20181)
#' y <- c(20203, 19794, 20174)
#' qtr.diff(x, y)
#' @export
qtr.diff <- function(yrq1, yrq2){
  ((get.y(yrq1) - get.y(yrq2)) * 4) + (get.q(yrq1) - get.q(yrq2))
}
