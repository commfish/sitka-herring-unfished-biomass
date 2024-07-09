#' Split spawner-recruit into strata
#'
#' This utility function splits spawner-recruit estimates into different strata,
#' listed in ascending order of mean recruit cohort size. All inputs should be
#' index by the same years
#'
#' @param recruits numeric vector of recruits
#' @param spawners numeric vector of spawners
#' @param strata factor vector delineating strata;
#' @return list of spawners, recruits, and their respective strata.
#' @export


split_sr <- function(recruits, spawners, strata){

  sr <- data.frame(recruits = recruits, spawners = spawners, strata = strata)

  split_order <- order(tapply(sr$spawners, INDEX = sr$strata, FUN = mean))
  sr_list <- split(sr, f = sr$strata)[split_order]

  return(sr_list)

}
