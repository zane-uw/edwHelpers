#' Create quoted character vectors
#'
#' @param ... text to be concatenated.
#' @return A quoted character vector.
#' @examples
#' Cs(some, pig)

Cs <- function(...) {as.character(sys.call())[-1]}



#' Make friendlier names for SDB fields
#'
#' @param x A vector of strings, typically the result of names(...).
#' @return A character vector in r-ish format, i.e. lowercase text with words separated by '.'.
#' @examples
#' fix.sdb.names(c("SDBSrcSysKey", "QuarterlyGPA", "AcademicYear"))

fix.sdb.names <- function(x = "EDWSampleVarName"){
  # convert UpperStyle edw/sdb names to r style
  tolower(gsub("([[:lower:]])([[:upper:]])", "\\1.\\2", x, fixed = F))
}
