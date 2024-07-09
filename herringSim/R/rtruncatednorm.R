#' Generate Sample from Truncated Normal Distribution
#'
#' This function generates a random sample from a pseudo-truncated normal
#' distribution. Values from a normal distribution that are outside of a user-
#' specified range are resampled. This occurs until no more values are outside
#' of the range. The truncated interval may be input as a numeric or as a
#' normal quantile. The default is to truncate outside of the lower 90%
#' of normal density.
#'
#'
#' @param n integer denoting number of observations in sample
#' @param ... Arguments passed to `rnorm`. Includes `mean` and `sd`.
#' @param lwr_quant Lower limit of normal density
#' @param upr_quant Upper limit of normal density
#' @param lwr Numeric giving lower limit to truncate past. Supersedes `lwr_quant`
#' if included
#' @param upr Numeric giving upper limit to truncate past. Supersedes `upr_quant`
#' if included
#' @return Numeric vector; truncated normal sample of size `n`
#'
#'
#' @export


rtruncatednorm <- function(n, ..., lwr_quant = .05, upr_quant = .95,
                           lwr = NULL, upr = NULL){

  if(is.null(lwr)) lwr <- qnorm(lwr_quant, ...)
  if(is.null(upr)) upr <- qnorm(upr_quant, ...)

  out <- rnorm(n, ...)

  while(sum(!(lwr < out & out < upr)) > 0){
    rows <- which(!(lwr < out & out < upr))
    out[rows] <- rnorm(length(rows), ...)
  }

  return(out)

}
