#' Mode
#'
#' Computes mode of a numeric vector
#'
#' @param x a numeric (or integer) vector
#' @return The modal value of `x`
#'
#' @export
Mode <- function(x) {
  ux <- unique(x)
  out <- ux[which.max(tabulate(match(x, ux)))]
  return(out)
}
