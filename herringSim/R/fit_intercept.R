#' Fit intercept-only stock-recruit model
#'
#' This function uses maximum likelihood to fit an intercept-only stock-recruitment
#' model.
#'
#' @param recruits numeric vector of estimates of annual recruits (millions)
#' @param spawners numeric vector of estimates for spawning biomass three years
#' prior (tonnes)
#' @param start numeric vector of starting values for model parameters
#' @param errorStructure string specifying the error structure of model
#'
#' @return A named list
#' - **coef:** Estimated model parameters
#' - **mod:** Function implementing model with estimated parameters
#' - **logLik:** Log-likelihood for model
#'
#' @export
#'


fit_intercept <- function(recruits, spawners, start,
                          errorStructure = c("additive", "multiplicative")){

  minuslogl <- function(logmu, logsigma2){
    mu <- exp(logmu)
    sigma2 <- exp(logsigma2)

    if(errorStructure == "additive"){
      preds <- rep(mu, length(recruits))
      minuslogl <- -1*sum(dnorm(recruits, mean = preds,
                                sd = sqrt(sigma2), log = TRUE))

    }
    if(errorStructure == "multiplicative"){
      preds <- rep(mu, length(recruits))
      minuslogl <- -1*sum(dnorm(log(recruits), mean = log(preds),
                                sd = sqrt(sigma2), log = TRUE))
    }
    return(minuslogl)
  }

  mleFit <- stats4::mle(minuslogl = minuslogl, start = start)

  coefOut <- exp(stats4::coef(mleFit))
  names(coefOut) <- c("mu", "sigma2")

  modOut <- function(x){
    muhat <- eval(coefOut)[1]
    return(muhat)
  }

  return(list(coef = coefOut,
              mod = modOut,
              logLik = stats4::logLik(mleFit),
              mleFit = mleFit))
}


