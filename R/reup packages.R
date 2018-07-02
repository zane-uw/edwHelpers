#' Bulk re-install/update
#'
#' @description Bulk re-install (or update) installed packages (should rarely be necessary)
#' @param
#' @return NA, quietly updates packages
#' @examples
#' bulk.reinstall()
#' @name bulk.reup
NULL

#' Reinstall packages
#' @rdname bulk.reinstall
#' @export
bulk.reinstall <- function() {
  pkgs <- installed.packages()
  ii <- is.na(pkgs[, "Priority"])
  pkgs <- pkgs[ii, 1]
  install.packages(pkgs)
}
