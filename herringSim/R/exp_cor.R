#' Exploratory Correlation Measure
#'
#' This function computes the exploratory correlation measure as described in
#' Williams & Quinn (2000).
#'
#'
#' @param x numeric vector for which correlation measure is desired.
#' @param y numeric vector for which correlation measure is desired. This vector
#' will be trimmed of outliers, hence it should correspond to a response variable
#' of interest.
#' @param method correlation method corresponding to those accepted by `cor` and
#' `cor.test`, i.e. one of `pearson`, `kendall`, or `spearman`.
#' @param trim_pct percentage of data to keep when trimming.
#' @return Exploratory correlation measure. Values outside of \eqn{\pm} 3 correspond to
#' p-values significant at an \eqn{\alpha}-level of 0.05
#'
#' @references \insertRef{williamsquinn2000}{herringSim}
#'
#' @export


exp_cor <- function(x, y, method = "kendall", trim_pct = 0.9){
  # computes the raw kendall tau
  cor_raw <- cor.test(x, y, method = method)
  p_raw <- cor_raw$p.value
  sign_raw <- ifelse(cor_raw$estimate < 0, -1, 1)

  # trims outliers (largest 10% of deviates from mean y)
  ord_deviates <- order(abs(mean(y) - y))
  trim_rows <- ord_deviates[1:round(quantile(ord_deviates, trim_pct))]

  # computes trimmed kendall tau
  cor_trim <- cor.test(x[trim_rows], y[trim_rows], method = method)
  p_trim <- cor_trim$p.value
  sign_trim <- ifelse(cor_trim$estimate < 0, -1, 1)

  # computes exploratory correlation measure
  out <- (sign_raw*log((1/p_raw) + 1) + sign_trim*log((1/p_trim) + 1)) / 2
  return(out)
}
