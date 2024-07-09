#' Fit ricker stock-recruit model
#'
#' This function uses maximum likelihood to fit a ricker stock-recruitment
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


fit_ricker <- function(recruits, spawners, start,
                       errorStructure = c("additive", "multiplicative")){

  minuslogl <- function(logalpha, logbeta, logsigma2){
    alpha <- exp(logalpha)
    beta <- exp(logbeta)
    sigma2 <- exp(logsigma2)

    if(errorStructure == "additive"){
      preds <- spawners * alpha * exp(-beta*spawners)
      minuslogl <- -1*sum(dnorm(recruits, mean = preds,
                                sd = sqrt(sigma2), log = TRUE))
    }
    if(errorStructure == "multiplicative"){
      logpreds <- log(spawners * alpha * exp(-beta*spawners))
      minuslogl <- -1*sum(dnorm(log(recruits), mean = logpreds,
                                sd = sqrt(sigma2), log = TRUE))
    }
    return(minuslogl)
  }

  mleFit <- stats4::mle(minuslogl = minuslogl, start = start)

  coefOut <- exp(stats4::coef(mleFit))
  names(coefOut) <- c("alpha", "beta", "sigma2")

  modOut <- function(x){
    alphahat <- eval(coefOut)[1]
    betahat <- eval(coefOut)[2]
    return(x * alphahat * exp(-betahat*x))
  }

  return(list(coef = coefOut,
              mod = modOut,
              logLik = stats4::logLik(mleFit),
              mleFit = mleFit))
}


