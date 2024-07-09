#' Akaike's Information Criterion (corrected)
#'
#' This function computes the AICc for a given model for which a basic AIC can
#' be obtained
#'
#'
#' @param mod A model object
#' @details The AICc is a small-sample corrected AIC computed using the following
#' formula: \deqn{AIC_c = AIC + \frac{2k^2 + 2k}{n-k-1}}
#' where k is the number of parameters in the model and n is the sample size. This
#' has the effect of choosing a more parsimonious model to avoid overfitting at
#' small sample sizes, but \eqn{AIC_c \rightarrow AIC} as \eqn{n \rightarrow \infty}
#'
#' @return A computed value for AICc
#'
#'
#' @export


AICc <- function(mod){
  aic <- AIC(mod)
  k <- length(coef(mod))
  n <- nobs(mod)
  out <- aic + ((2*(k^2) + (2*k))/(n-k-1))
  return(out)
}
