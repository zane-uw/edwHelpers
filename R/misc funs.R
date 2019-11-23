# Miscellaneous functions ---------------------------------------------

# location of single column (internal function)
col.loc <- function(df, x){ return( which(names(df) == x) ) }


#' Many tables are unused (undocumented) or constant. Remove them.
#'
#' @param df a dataframe.
#' @return A dataframe w/o zero variance or constant columns.
#'
#' @export rm.empty
rm.empty <- function(df){
  i <- apply(df, 2, function(x) length(unique(x))) == 1
  return(df[,i == F])
}

#' Convert other formats to binary flags.
#'
#' @section Description:
#' Flags/indicators are stored in different formats. This function converts two arbitrary values to 0/1. Others will be converted to NA.
#' The default behavior ignores case, e.g. binarize.vals(x, 'Y', 'N') will convert 'Y' to 0 but 'y' to NA. Setting ignore.case = FALSE will first convert all
#'
#' @usage binarize.vals(x, val0, val1, ignore.case = TRUE)
#'
#' @param x a vector.
#' @param val0,val1 the values to convert to 0/1 respectively.
#' @param ignore.case if TRUE, conversion will ignore
#' @param as.numeric if TRUE, return a numeric vector else return a string.
#' @return A new vector of zeroes and ones.
#'
#' @section Warning:
#' TRUE is not 'true'! Setting ignore.case = FALSE when converting T/F will not work as desired.
#' The function should catch this by checking is.logical(x).
#'
#' @examples
#' x <- c(sample(c('Y', 'N'), 5, replace = T), 'a', 'y', 0, 'n')
#' print(x)
#' binarize.vals(x, 'Y', 'N')
#'
#' x <- c(T, F, T, T, F)
#' binarize.vals(x, 'T', 'F', ignore.case = F)  # wrong
#'
#' @export binarize.vals
binarize.vals <- function(x, val0, val1, ignore.case = TRUE, as.numeric = TRUE){
  if(is.logical(x) & ignore.case == F){
    ignore.case <- T
    print('Found and fixed logical vec w/ ignore.case = F')
  }

  if(!ignore.case){
    x <- tolower(x)
    val0 <- tolower(val0)
    val1 <- tolower(val1)
  }

  y <- rep(NA, length.out = length(x))
  y[x == val0] <- 1
  y[x == val1] <- 0

  if(!as.numeric){
    return(as.character(y))
  }
  return(y)
}


#' Return column index
#'
#' @section Description:
#' Sometimes it is useful to find or pass columns by position. This is a simple shortcut that returns the numeric index of a data frame column (or columns).
#' Calls internal col.loc() function (the original 1-variable version of varwhich)
#'
#' @param df A data frame
#' @param x A (quoted) column name
#'
#' @example
#' varwhich(mtcars, 'cyl')
#' @export varwhich
varwhich <- function(df, x){
  if(length(x > 1)){
    res <- vector('integer')
    for(i in 1:length(x)){
      res <- c(res, col.loc(df, x[i]))
    }
    return(res)
  }
  return(col.loc(df, x))
}
