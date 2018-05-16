#' Create quoted character vectors
#'
#' @param ... text to be concatenated.
#' @return A quoted character vector.
#'
#' @examples
#' Cs(some, pig)
#'
#' @export Cs
Cs <- function(...) {as.character(sys.call())[-1]}



#' Make friendlier names for SDB fields
#'
#' @param x A vector of strings, typically the result of names(...).
#' @return A character vector in r-ish format, i.e. lowercase text with words separated by '.'.
#'
#' @examples
#' fix.sdb.names(c("SDBSrcSysKey", "QuarterlyGPA", "AcademicYear"))
#'
#' @export fix.sdb.names
fix.sdb.names <- function(x = "EDWSampleVarName"){
  # convert UpperStyle edw/sdb names to r style
  tolower(gsub("([[:lower:]])([[:upper:]])", "\\1.\\2", x, fixed = F))
}

#' Trim excess whitespace from data frame cols
#'
#' @param df A data.frame
#' @return A data.frame with whitespace trimmed from characters columns.
#'
#' @examples
#' x <- data.frame("a" = c(" Wilbur  ", "Charlotte A. Cavatica  ", "  Fern Arable   ", " Templeton  "), "b" = 1:4)
#' trim.white(x)
#' @export trim.white
trim.white <- function(df){
  i <- sapply(df, is.character)
  df[i] <- lapply(df[i], trimws, which = 'both')
  return(df)
}
