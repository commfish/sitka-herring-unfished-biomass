#' Fit shepherd stock-recruit model
#'
#' This function uses maximum likelihood to fit a shepherd stock-recruitment
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


fit_shepherd <- function(recruits, spawners, start,
                         errorStructure = c("additive", "multiplicative")){

  minuslogl <- function(logalpha, logbeta, loggamma, logsigma2){

    alpha <- exp(logalpha)
    beta <- exp(logbeta)
    gamma <- exp(loggamma)
    sigma2 <- exp(logsigma2)

    if(errorStructure == "additive"){
      preds <- spawners * alpha * (1 / (1+(spawners/beta)^gamma))
      minuslogl <- -1*sum(dnorm(recruits, mean = preds,
                                sd = sqrt(sigma2), log = TRUE))
    }
    if(errorStructure == "multiplicative"){
      logpreds <- log(spawners * alpha *
                        (1 / (1+(spawners/beta)^gamma)))
      minuslogl <- -1*sum(dnorm(log(recruits), mean = logpreds,
                                sd = sqrt(sigma2), log = TRUE))
    }

    return(minuslogl)

  }

  mleFit <- stats4::mle(minuslogl = minuslogl, start = start)

  coefOut <- exp(stats4::coef(mleFit))
  names(coefOut) <- c("alpha", "beta", "gamma", "sigma2")

  modOut <- function(x){
    alphahat <- eval(coefOut)[1]
    betahat <- eval(coefOut)[2]
    gammahat <- eval(coefOut)[3]
    return(x * alphahat * (1 / (1+(x/betahat)^gammahat)))
  }

  return(list(coef = coefOut,
              mod = modOut,
              logLik = stats4::logLik(mleFit)))
}


