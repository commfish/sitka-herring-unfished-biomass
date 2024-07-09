#' Initialize data frame to simulate unfished biomass
#'
#' This function initializes a data frame to be used by any of [B_sim_empirical()],
#' [B_sim_lnRecruitment()], or [B_sim_functional()]. The data frame is mostly `NA`'s
#' to be filled by other functions with simulated herring counts; the first 8 years
#' of age-3 recruit counts are sampled from the provided vector of `recruits`. Then
#' counts for every year thereafter as the `recruits` age are computed from the
#' constant survival rate `S`.
#'
#'
#' @param recruits numeric vector of estimates of annual recruits (millions)
#' @param S estimated constant survival rate for all age classes
#' @return A data frame made up of the following vectors
#' - **age_3:** vector of age 3 recruits
#' - **age_4:** vector of age 4 herring
#' - **age_5:** vector of age 5 herring
#' - **age_6:** vector of age 6 herring
#' - **age_7:** vector of age 7 herring
#' - **age_8:** plus group vector of age 8+ herring
#'
#' @export



B_sim_init <- function(recruits, S){

  ## generate sample of 5 years of age-3 recruits.

  # These will be used to populate all other age classes over 5 years

  N_t <- data.frame(age3 = rep(NA, 5+8))
  N_t$age3[1:8] <- sample(recruits, 8, replace = TRUE)

  # note: the 8 is to allow for 5 years to populate the remaining age classes so
  # there are 3 years of spawner biomasses to apply the empiracal spawner-recruit
  # relationship prior to simulation

  ## populate all other age classes for first 5 years

  N_t$age8 <- N_t$age7 <- N_t$age6 <- N_t$age5 <- N_t$age4 <- NA

  # the following loop computes each age class from the year prior using constant
  # survival for the first 13 years

  for(t in 1:(5+8)){
    # 13 = 5+8 so that each age class has 8 numeric entries
    for(j in 2:ncol(N_t)){
      N_t[t, j] <- ifelse(!is.null(1+t-j), N_t[t-1, j-1]*S, NA)
      # this indexes the columns so each age class is computed from the year prior
      # Na's given to older age classes in earlier years
      if(t > 6){
        N_t$age8[t] <- (N_t$age8[t-1] + N_t$age7[t-1]) * S
        # age7 -> age8 for the first year (t = 6)
        # age7 + age8 -> age8 for every year after (plus group accounted for)
      }
    }
  }

  return(N_t)
}
