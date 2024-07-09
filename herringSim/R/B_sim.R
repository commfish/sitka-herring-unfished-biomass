#' Simulate average unfished biomass
#'
#' This function simulates the average unfished biomass of herring using a
#' user-specified spawner-recruit relationship and data including weight-at-age,
#' proportion of stock that are mature, and a constant survival rate.
#'
#' @param recruits numeric vector of estimates of annual recruits (millions)
#' @param spawners numeric vector of estimates for spawning biomass three years
#' prior (tonnes)
#' @param strata factor vector giving the strata for the empirical spawner-recruit
#' relationship
#' @param mean_wt numeric vector of mean weight-at-age for ages 3 to 8+
#' @param maturity numeric vector of estimated maturity-at-age for ages 3 to 8+
#' @param S estimated constant survival rate for all age classes
#' @param num_years Integer; the number of years to simulate to
#' @param model_type The type of spawner-recruit model. If empirical is chosen,
#' then age-3 recruits are randomly sampled from strata based on spawning biomass
#' three years prior. If some other character string is given, then the string
#' must correspond to the name of a functional spawner-recruit model made elsewhere
#' @param lnorm_err A lognormal error term to be used, if desired, if using a
#' functional spawner-recruit model.
#' @return A data frame made up of the following vectors
#' - **YEAR:** Index of simulated year
#' - **B:** Simulated biomass (tons) for that year
#' - **B_cumavg:** The cumulative average biomass (tons) up to that year
#' - **AUB.25:** The estimated 25 percent management threshold for entire series
#' - **AUB:** The estimated average unfished biomass
#'
#' @export



B_sim <- function(recruits, spawners, strata = NA, mean_wt, maturity, S,
                  num_years = 2500,
                  model_type = c("empirical", "empirical2"),
                  lnorm_err = NULL){

  sr <- data.frame(recruits = recruits, spawners = spawners, strata = strata)

  ## generate sample of 5 years of age-3 recruits.

  # These will be used to populate all other age classes over 5 years

  N_t <- data.frame(age3 = rep(NA, num_years + 8))
  N_t$age3[1:8] <- sr$recruits[sample(seq(recruits), 8, replace = TRUE)]

  # note: the 8 is to allow for 5 years to populate the remaining age classes so
  # there are 3 years of spawner biomasses to apply the empiracal spawner-recruit
  # relationship prior to simulation

  ## populate all other age classes for first 5 years

  N_t$age8 <- N_t$age7 <- N_t$age6 <- N_t$age5 <- N_t$age4 <- NA

  # the following loop computes each age class from the year prior using constant
  # survival for the first 13 years

  for(t in 1:13){
    # 13 = 8+5 so that each age class has 8 numeric entries
    for(j in 2:ncol(N_t)){
      N_t[t, j] <- ifelse(!is.null(1+t-j), N_t[t-1, j-1]*S, NA)
      # this indexes the columns so each age class is computed from the year prior
      # Na's given to older age classes in earlier years
      if(t > 6){
        N_t$age8[t] <- (N_t$age8[t-1] + N_t$age7[t-1]) * S
        # age7 -> age8 for the first year (t = 6)
        # age7 + age8 -> age8 for every year after
      }
    }
  }

  ## compute biomass for first 8 years

  N_t$B_tonnes <- apply(as.matrix(N_t[,1:6]), MARGIN = 1,
                        FUN = function(x) x %*% as.matrix(mean_wt*maturity))

  # simulation

  if(grepl(pattern = "empirical", x = model_type)){

    if(length(unique(strata)) > 1){

      sr_list <- split_sr(sr$recruits, sr$spawners, sr$strata)
      splits <- strata_boundaries(sr_list)

    }

  }

  if(model_type == "empirical"){

    for(t in 9:nrow(N_t)){
      # this if else statement identifies B[y-3] and samples recruits accordingly

      if(length(unique(strata)) == 1){

        N_t$age3[t] <- sample(sr[,"recruits"], 1)

      } else if(length(unique(strata)) == 2){

        if(N_t$B_tonnes[t-3] < splits){
          N_t$age3[t] <- sample(sr_list[[1]][,"recruits"], 1)
        } else if(splits <= N_t$B_tonnes[t-3]){
          N_t$age3[t] <- sample(sr_list[[2]][,"recruits"], 1)
        }

      } else if(length(unique(strata)) >= 3){

        if(N_t$B_tonnes[t-3] < splits[1]){
          N_t$age3[t] <- sample(sr_list[[1]][,"recruits"], 1)
        } else if((splits[1] <= N_t$B_tonnes[t-3]) &
                  (N_t$B_tonnes[t-3] < splits[length(sr_list)-1])){
          for(j in 2:(length(sr_list)-1)){
            if((splits[j-1] <= N_t$B_tonnes[t-3]) & (N_t$B_tonnes[t-3] < splits[j])){
              N_t$age3[t] <- sample(sr_list[[j]][,"recruits"], 1)
            } else {
              next
            }
          }
        } else if(splits[length(sr_list)-1] <= N_t$B_tonnes[t-3]){
          N_t$age3[t] <- sample(sr_list[[length(sr_list)]][,"recruits"], 1)
        }
      }

      # compute biomass
      N_t$B_tonnes[t] <- as.matrix(N_t[t,1:6]) %*% as.matrix(mean_wt*maturity)
      # compute other age classes
      ifelse(t+1<=nrow(N_t), N_t$age4[t+1] <- N_t$age3[t] * S, next)
      ifelse(t+2<=nrow(N_t), N_t$age5[t+2] <- N_t$age4[t+1] * S, next)
      ifelse(t+3<=nrow(N_t), N_t$age6[t+3] <- N_t$age5[t+2] * S, next)
      ifelse(t+4<=nrow(N_t), N_t$age7[t+4] <- N_t$age6[t+3] * S, next)
      # age7 + age8 -> age8
      ifelse(t+5<=nrow(N_t), N_t$age8[t+5] <- (N_t$age7[t+4] + N_t$age8[t+4]) * S, next)
    }

  } else if(model_type == "empirical2"){

    ifelse(exists("sr_list"),
                   Rbar <- lapply(sr_list, FUN = function(x) mean(x[,"recruits"])),
                   Rbar <- mean(sr$recruits))

    weight <- 0.5

    for(t in 9:nrow(N_t)){
      sigma_Rec <- runif(1, min = .0001, max = 2)

      if(length(unique(strata)) == 1){

        N_t$age3[t] <- Rbar * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)

      } else if(length(unique(strata)) == 2){

        if(N_t$B_tonnes[t-3] < splits){
          N_t$age3[t] <- Rbar[[1]] * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)
        } else if(splits <= N_t$B_tonnes[t-3]){
          N_t$age3[t] <- Rbar[[2]] * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)
        }

      } else if(length(unique(strata)) >= 3){

        if(N_t$B_tonnes[t-3] < splits[1]){
          N_t$age3[t] <- Rbar[[1]] * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)
        } else if((splits[1] <= N_t$B_tonnes[t-3]) &
                  (N_t$B_tonnes[t-3] < splits[length(sr_list)-1])){
          for(j in 2:(length(sr_list)-1)){
            if((splits[j-1] <= N_t$B_tonnes[t-3]) & (N_t$B_tonnes[t-3] < splits[j])){
              N_t$age3[t] <- Rbar[[j]] * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)
            } else {
              next
            }
          }
        } else if(splits[length(sr_list)-1] <= N_t$B_tonnes[t-3]){
          N_t$age3[t] <- Rbar[[length(sr_list)]] * exp(rnorm(1, sd = sigma_Rec^2) - weight*sigma_Rec^2)
        }

      }

      if(N_t$age3[t] > max(sr$recruits)) N_t$age3[t] <- max(sr$recruits)
      # compute biomass
      N_t$B_tonnes[t] <- as.matrix(N_t[t,1:6]) %*% as.matrix(mean_wt*maturity)
      # compute other age classes
      ifelse(t+1<=nrow(N_t), N_t$age4[t+1] <- N_t$age3[t] * S, next)
      ifelse(t+2<=nrow(N_t), N_t$age5[t+2] <- N_t$age4[t+1] * S, next)
      ifelse(t+3<=nrow(N_t), N_t$age6[t+3] <- N_t$age5[t+2] * S, next)
      ifelse(t+4<=nrow(N_t), N_t$age7[t+4] <- N_t$age6[t+3] * S, next)
      # age7 + age8 -> age8
      ifelse(t+5<=nrow(N_t), N_t$age8[t+5] <- (N_t$age7[t+4] + N_t$age8[t+4]) * S, next)
    }

  } else if(!grepl(pattern = "empirical", x = model_type)){

    if(!exists(model_type)) stop(paste(model_type, "model not found!"))

    for(t in 9:nrow(N_t)){
      # this lines predicts age-3 recruits from B[y-3] spawner biomass using sr model
      N_t$age3[t] <- exp(predict(eval(as.name(model_type)),
                                 newdata = list(spawners = N_t[t-3, "B_tonnes"])))
      if(!is.null(lnorm_err)){
        N_t$age3[t] <- N_t$age3[t] + rlnorm(1, 0, lnorm_err)
      }
      # compute biomass
      N_t$B_tonnes[t] <- as.matrix(N_t[t,1:6]) %*% as.matrix(mean_wt*maturity)
      # compute other age classes
      ifelse(t+1<=nrow(N_t), N_t$age4[t+1] <- N_t$age3[t] * S, next)
      ifelse(t+2<=nrow(N_t), N_t$age5[t+2] <- N_t$age4[t+1] * S, next)
      ifelse(t+3<=nrow(N_t), N_t$age6[t+3] <- N_t$age5[t+2] * S, next)
      ifelse(t+4<=nrow(N_t), N_t$age7[t+4] <- N_t$age6[t+3] * S, next)
      # age7 + age8 -> age8
      ifelse(t+5<=nrow(N_t), N_t$age8[t+5] <- (N_t$age7[t+4] + N_t$age8[t+4]) * S, next)

    }

  }

  N_t$B_tons <- N_t$B_tonnes * 1.102311  # convert to us tons (1 tonne = 1.102311 ton)

  ## compute cumulative average biomass in tons

  B <- data.frame(YEAR = 1:num_years, B = N_t[-8:-1,"B_tons"])
  B$B_cumavg <- cumsum(B$B) / seq_along(B$B)

  # compute AUB / .25*AUB for years after convergence

  year_converges <- find_convergence(B$B_cumavg)

  # compute AUB and threshold after convergence year

  B$AUB <- B$AUB.25 <- NA
  B$AUB[B$YEAR > year_converges] <- mean(B$B[B$YEAR > year_converges])
  # estimated AUB of 80k tons

  aub <- unique(na.omit(B$AUB))
  threshold <- 0.25*aub

  B$AUB.25[B$YEAR > year_converges] <- threshold

  return(B)
}
