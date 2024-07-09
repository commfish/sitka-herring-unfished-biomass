#' Generate Recruits
#'
#' This utility function generates age-3 herring recruits from a lognormal
#' distribution using the observed standard deviations of stratified spawner-
#' recruit parameter estimates of a herring stock.
#'
#' Recruitment is treated as a quasi-spawner-independent process. The spawner-
#' recruit estimates may or may not be stratified. Within each strata, the mean
#' recruitment multiplied by a log-normal error, where the variance term is
#' computed from the data within each strata.
#'
#' @param recruits numeric vector of estimates of annual recruits (millions)
#' @param spawners numeric vector of estimates for spawning biomass three years
#' prior (metric tons)
#' @param strata factor vector giving the strata for the empirical spawner-recruit
#' relationship
#' @param Rbar list of mean recruitment for each strata
#' @param sigmahat2 list of observed standard deviations for each strata
#' @param Nt3 numeric, spawning biomass in metric tons
#' @return A numeric scalar giving the number of age-3 herring recruits, in millions
#' @references \insertRef{methot2011}{herringSim}
#'
#' @export
#'

R_gen <- function(recruits, spawners, strata = NA, Rbar, sigmahat2, Nt3){

  weight <- 0.5

  sr <- data.frame(recruits = recruits,
                   spawners = spawners,
                   strata = as.numeric(strata))

  if(length(unique(sr$strata)) > 1){

    sr_list <- split_sr(recruits = sr$recruits, spawners = sr$spawners,
                        strata = sr$strata)
    splits <- strata_boundaries(sr_list)

  }

  if(length(unique(sr$strata)) == 1){

    R <- Rbar * exp(rnorm(1, sd = sqrt(unlist(sigmahat2))) -
                                weight*unlist(sigmahat2))

  } else if(length(unique(sr$strata)) == 2){

    if(Nt3 < splits){
      R <- Rbar[[1]] * exp(rnorm(1, sd = sqrt(sigmahat2[[1]])) -
                                       weight*sigmahat2[[1]])
    } else if(splits <= Nt3){
      R <- Rbar[[2]] * exp(rnorm(1, sd = sqrt(sigmahat2[[2]])) -
                                       weight*sigmahat2[[2]])
    }

  } else if(length(unique(sr$strata)) >= 3){

    # these if statements identifies B[y-3] and sample recruits accordingly

    if(Nt3 < splits[1]){
      R <- Rbar[[1]] * exp(rnorm(1, sd = sqrt(sigmahat2[[1]])) -
                                       weight*sigmahat2[[1]])
    } else if((splits[1] <= Nt3) &
              (Nt3 < splits[length(sr_list)-1])){
      for(j in 2:(length(sr_list)-1)){
        if((splits[j-1] <= Nt3) & (Nt3 < splits[j])){
          R <- Rbar[[j]] * exp(rnorm(1, sd = sqrt(sigmahat2[[j]])) -
                                           weight*sigmahat2[[j]])
        } else {
          next
        }
      }
    } else if(splits[length(sr_list)-1] <= Nt3){
      R <- Rbar[[length(sr_list)]] *
        exp(rnorm(1, sd = sqrt(sigmahat2[[length(sr_list)]])) -
              weight*sigmahat2[[length(sr_list)]])
    }

  }

  return(R)

}
