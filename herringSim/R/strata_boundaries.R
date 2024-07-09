#' Compute strata boundaries
#'
#' This utility function computes the strata boundaries delineating the strata
#' used in the empirical spawner-recruit relationship. The boundaries are computed
#' as the mean between the minimum and maximum biomass of adjacent strata.
#'
#' @param sr_list list of spawner-recruit estimates (and strata) as returned by
#' the function split_sr
#' @return Vector of spawner biomasses that delineate strata boundaries
#' @references
#'   \insertRef{carlile1998}{herringSim}
#' @export

strata_boundaries <- function(sr_list){

  splits <- c()
  for(i in 1:(length(sr_list)-1)){
    splits[i] <- mean(c(max(sr_list[[i]][,"spawners"]),
                        min(sr_list[[i+1]][,"spawners"])))
  }
  return(sort(splits))

}
