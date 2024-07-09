#################################################################################

# Compare forecast methods with ASA 2022-forecast operating model and varying intervals

# Implementation of framework from Van Beveren et. al (2021) for comparing the
# ability of various recruitment forecasting methods to produce accurate estimates of
# spawning biomass of a stock. 

# Here, the general idea is to load outputs from the 
# Sitka Sound ASA 2022-forecast model and treat them as the true state of the 
# stock (i.e. the "operating model"). Then the final delta years of the operating 
# model time series are truncated and estimated; recruitment is iteratively forecasted,
# numbers-at-age computed, catch removed, and biomass computed. 

# This procedure is referred to as Operating Model Forecasting Procedure (OMFP)

# Recruitment forecasting methods:
# model-based: intercept-only model, ricker, bev-holt, environmental rickers,
# sampling-based: single-stratum sampling, 3-strata sampling, time-tapered sampling
# other: simplex

# ASA 2022-forecast 
# spawn years 1976-2019
# recruit years 1979-2022

# data source: 
# V:\Region1Shared-DCF\Research\Herring-Dive Fisheries\Herring\Year 2022 Forecasts\Forecast Models\Sitka\Results-save tpl wo bootstrap output and fig w new CI\model.49_311_best\Report.xlsx

# Inputs: 
#   - pdo dataset recruitment_comparisons/data/pdo_2000-2022.txt
#   - ERSST dataset recruitment_comparisons/data/sst_mon.csv
#   - Stock assessment model inputs and outputs. Data found in shared drive above
#     but read in via herringSim package

# Outputs:
# This script saves and RDS file at `data\analysis_improvements_v2\sr_sim.rds`. This
# file is a list where each element is a data frame associated with a different 
# value of delta (i.e. different ranges of years in the operating model being estimated).
# Each data frame contains estimates for recruits and spawning biomass (with confidence
# intervals) for each recruitment forecasting method.

# Approximate runtime: 30 minutes

# by: CL Roberts

#################################################################################

## set up

start_time <- Sys.time()

library(herringSim)
library(ggplot2)
library(stats4)
library(dplyr)
library(data.table)
library(ggrepel)

max_out <- 15
final_year <- max(caa_asa2023_forecast$Year)
sr_sim <- vector(mode = "list", length = max_out)
recruit_year <- 1979:final_year

# sr <- sr_asa2021 |> mutate(spawners = spawners / 1.102311) # convert to metric tons
# sr$cycle <- factor(rep(c(1,0,0,0), length.out = nrow(sr)))

#--------------------------------------------------------------------------------

## data wrangling

# use parameter estimates for data ranges covered by operating model

# mean weight-at-age
mean_wt <- mean_wt_asa2023_forecast |> 
                filter(1979 <= rownames(mean_wt_asa2023_forecast)) 
# maturity-at-age
maturity <- maturity_asa2023_forecast |>
                filter(1979 <= rownames(maturity_asa2023_forecast))
# survival rates
S <- S_asa2023_forecast |> 
                filter(1979 <= rownames(S_asa2023_forecast)) |>
                colMeans() |> 
                unique()
# pre-harvest numbers-at-age
naa <- naa_asa2023_forecast |>
            filter(1979 <= Year)
# catch-at-age
caa <- caa_asa2023_forecast |>
                filter(1979 <= caa_asa2023_forecast$Year) 
         

# true spawners and recruits for each recruit year

sr <- data.frame(recruits_true = naa_asa2023_forecast[naa_asa2023_forecast$Year %in% recruit_year,"age3"],
                 spawners_true = rowSums(((naa_asa2023_forecast[,-1]*maturity_asa2023_forecast)-caa_asa2023_forecast[,-1]) * mean_wt_asa2023_forecast)[naa_asa2023_forecast$Year %in% (recruit_year-3)],
                 year = naa_asa2023_forecast$Year[naa_asa2023_forecast$Year %in% recruit_year])
sr$recruits <- sr$recruits_true
sr$spawners <- sr$spawners_true

sr <- sr |> filter(year >= 1979)

# spawners_true <- rowSums(((naa_asa2021[,-1]*maturity_asa2021)-caa_asa2021[,-1]) * mean_wt_asa2021)
# names(spawners_true) <- caa_asa2021[,"year"]
# sr$spawners_true <- spawners_true[names(spawners_true) %in% 1976:2018]

# these calculations validate with results from spreadsheet 

# naa[,-1]*maturity  # mature numbers-at-age
# ((naa[,-1]*maturity)-caa[,-1])  # spawning numbers-at-age (post-catch)
# naa[,-1]*maturity * mean_wt  # mature biomass-at-age (metric tons)
# ((naa[,-1]*maturity)-caa[,-1]) * mean_wt  # spawning biomass-at-age (metric tons)
# rowSums(((naa[,-1]*maturity)-caa[,-1]) * mean_wt) # total spawning B by year (metric tons)


#--------------------------------------------------------------------------------

## read in environmental covariates; see ./sr_mods.r for more information regarding data sources

# sea surface temperature (sst)

sst <- read.csv("recruitment_comparisons/data/sst_mon.csv") |>
        as.data.table() |>
        dcast(Year ~ Month, value.var = "SST") |>
        select(Year, Apr, May)
sst$spring_mean <- rowMeans(sst[,-1])

# upwelling index (upw)

# upw_raw <- read.table("recruitment_comparisons/data/upw_index_1967-2022.txt", header = TRUE) |>
#         mutate(date = format(as.Date(as.character(YYYYMMDD), "%Y%m%d"), "%Y-%m"),
#                year = substr(YYYYMMDD, 1, 4)) |>
#         as.data.table()

# upw_mo_means <- tapply(upw_raw$Index, INDEX = upw_raw$date, FUN = mean)

# upw <- data.table(matrix(nrow = length(unique(upw_raw$year)), ncol = 12))
# for(i in 1:12){
#         if(i < 10){
#                 upw[,i] <- upw_mo_means[grep(paste0("-0", i), names(upw_mo_means))]
#         } else if(i >= 10){
#                 upw[,i] <- upw_mo_means[grep(paste0("-", i), names(upw_mo_means))]
#         }
# }
# colnames(upw) <- c(month.abb)

# upw$ann_mean <- rowMeans(upw)
# upw$Year <- as.integer(unique(upw_raw$year))

# southern oscillation index

# soi <- read.table("recruitment_comparisons/data/soi_1951-2022.txt", 
#                   header = TRUE, col.names = c("Year", month.abb)) |>
#         as.data.table()

# soi$ann_mean <- rowMeans(soi[, ..month.abb])

# # pacific decadal oscillation

pdo <- read.table("recruitment_comparisons/data/pdo_1900-2022.txt", header = TRUE) |>
        as.data.table() |>
        select(Year, Apr, May)

pdo$spring_mean <- rowMeans(pdo[, -1])

# precip

# precip <- read.table("recruitment_comparisons/data/precip_mon.txt") |>
#         as.data.table() |>
#         dcast(Year ~ Month, value.var = "precip") |>
#         select(Year, all_of(month.abb))

# precip$Dec[48] <- mean(na.omit(precip$Dec))
# precip$ann_means <- rowMeans(precip[, ..month.abb])

#------------------------------------------------------------------------------------------

## for simplex forecasting

E <- 5
k <- E+1
Embed <- function(X, dimension=E) {
    E_X <- matrix(NA, nrow = length(X)-dimension+1, ncol = dimension)
    for(i in 1:dimension){
        E_X[,i] <- X[i:(length(X)-dimension+i)]
    }
    return(E_X)
}

#--------------------------------------------------------------------------------

################################# Begin OMFP ####################################

# this loop performs OMFP for delta = 4, 5, 6, ..., 23
# I.e. spawning biomass in the operating model is estimated (for each recruitment 
# forecasting method) for the last year, then last two years, then three ...

for(l in 1:max_out){

    ## set up list elements to be filled in

    delta <- l+3  # number of years to forecast until present

    sr_sim[[l]] <- sr[1:(nrow(sr)-delta), c("year", rep(c("spawners", "recruits"), 9))]
    colnames(sr_sim[[l]]) <- c("year", 
                            "spawners_int", "recruits_int",
                            "spawners_bh", "recruits_bh",
                            "spawners_rick", "recruits_rick", 
                            "spawners_rick_sst", "recruits_rick_sst",
                            # "spawners_rick_upw", "recruits_rick_upw",
                            # "spawners_rick_soi", "recruits_rick_soi",
                            "spawners_rick_pdo", "recruits_rick_pdo",
                            # "spawners_rick_precip", "recruits_rick_precip",
                            "spawners_samp1", "recruits_samp1",
                            "spawners_samp3", "recruits_samp3",
                            "spawners_samptt", "recruits_samptt",
                            # "spawners_kern", "recruits_kern",
                            "spawners_simp", "recruits_simp")

    sr_sim[[l]]$int_upr <- sr_sim[[l]]$int_lwr <- NA
    sr_sim[[l]]$bh_upr <- sr_sim[[l]]$bh_lwr <- NA
    sr_sim[[l]]$rick_upr <- sr_sim[[l]]$rick_lwr <- NA
    sr_sim[[l]]$rick_sst_upr <- sr_sim[[l]]$rick_sst_lwr <- NA
    # sr_sim[[l]]$rick_upw_upr <- sr_sim[[l]]$rick_upw_lwr <- NA
    # sr_sim[[l]]$rick_soi_upr <- sr_sim[[l]]$rick_soi_lwr <- NA
    sr_sim[[l]]$rick_pdo_upr <- sr_sim[[l]]$rick_pdo_lwr <- NA
    # sr_sim[[l]]$rick_precip_upr <- sr_sim[[l]]$rick_precip_lwr <- NA
    sr_sim[[l]]$samp1_upr <- sr_sim[[l]]$samp1_lwr <- NA
    sr_sim[[l]]$samp3_upr <- sr_sim[[l]]$samp3_lwr <- NA
    sr_sim[[l]]$samptt_upr <- sr_sim[[l]]$samptt_lwr <- NA
    # sr_sim[[l]]$kern_upr <- sr_sim[[l]]$kern_lwr <- NA
    sr_sim[[l]]$simp_upr <- sr_sim[[l]]$simp_lwr <- NA

    #-----------------------------------------------------------------------------

    ## initialize numbers-at-age matrices for competing recruitment methods

    forecast_rows <- (nrow(sr)-delta+1):nrow(sr)
    forecast_years <- sr$year[forecast_rows]

    # numbers-at-age
    N_t_int <- N_t_bh <- N_t_rick <- N_t_rick_sst <- N_t_rick_pdo <- naa |> select(!Year)
    # N_t_rick_upw <- N_t_rick_soi <- N_t_rick_precip <- N_t_kern <- naa |> select(!Year)
    N_t_samp1 <- N_t_samp3 <- N_t_samptt <- N_t_simp <-  naa |> select(!Year)

    N_t_int[forecast_rows,] <- NA   # intercept-only model
    N_t_bh[forecast_rows,] <- NA   # beverton-holt
    N_t_rick[forecast_rows,] <- NA   # ricker 
    N_t_rick_sst[forecast_rows,] <- NA  # ricker with sea-surface temp
    # N_t_rick_upw[forecast_rows,] <- NA  # ricker with upwelling index
    # N_t_rick_soi[forecast_rows,] <- NA  # ricker with southern oscillation index
    N_t_rick_pdo[forecast_rows,] <- NA  # ricker with pacific decadal index
    # N_t_rick_precip[forecast_rows,] <- NA  # ricker with precipitation
    N_t_samp1[forecast_rows,] <- NA  # empirical with 1 strata
    N_t_samp3[forecast_rows,] <- NA  # empirical with 3 strata
    N_t_samptt[forecast_rows,] <- NA  # time-tapered sampling
    # N_t_kern[forecast_rows,] <- NA  # sample kernel density
    N_t_simp[forecast_rows,] <- NA  # simplex forecasting

    # for lower confidence bounds
    N_t_int_lwr <- N_t_bh_lwr <- N_t_rick_lwr <- N_t_rick_sst_lwr <- N_t_rick_pdo_lwr <-  naa |> select(!Year)
    # N_t_rick_upw_lwr <- N_t_rick_soi_lwr <- N_t_rick_precip_lwr <- N_t_kern_lwr <-  naa |> select(!Year)
    N_t_samp1_lwr <- N_t_samp3_lwr <- N_t_samptt_lwr <- naa |> select(!Year)
    # N_t_simp_lwr <- naa |> select(!Year)
   
    N_t_int_lwr[forecast_rows,] <- NA
    N_t_bh_lwr[forecast_rows,] <- NA
    N_t_rick_lwr[forecast_rows,] <- NA
    N_t_rick_sst_lwr[forecast_rows,] <- NA
    # N_t_rick_upw_lwr[forecast_rows,] <- NA
    # N_t_rick_soi_lwr[forecast_rows,] <- NA
    N_t_rick_pdo_lwr[forecast_rows,] <- NA
    # N_t_rick_precip_lwr[forecast_rows,] <- NA
    N_t_samp1_lwr[forecast_rows,] <- NA
    N_t_samp3_lwr[forecast_rows,] <- NA
    N_t_samptt_lwr[forecast_rows,] <- NA
    # N_t_kern_lwr[forecast_rows,] <- NA
    # N_t_simp_lwr[forecast_rows,] <- NA

    # for upper confidence bounds
    N_t_int_upr <- N_t_bh_upr <- N_t_rick_upr <- N_t_rick_sst_upr <- N_t_rick_pdo_upr <- naa |> select(!Year)
    # N_t_rick_upw_upr <- N_t_rick_soi_upr <-  N_t_rick_precip_upr <- N_t_kern_upr <- naa |> select(!Year)
    N_t_samp1_upr <- N_t_samp3_upr <- N_t_samptt_upr <- naa |> select(!Year)
    # N_t_simp_upr <- naa |> select(!Year)
   
    N_t_int_upr[forecast_rows,] <- NA
    N_t_bh_upr[forecast_rows,] <- NA
    N_t_rick_upr[forecast_rows,] <- NA
    N_t_rick_sst_upr[forecast_rows,] <- NA
    # N_t_rick_upw_upr[forecast_rows,] <- NA
    # N_t_rick_soi_upr[forecast_rows,] <- NA
    N_t_rick_pdo_upr[forecast_rows,] <- NA
    # N_t_rick_precip_upr[forecast_rows,] <- NA
    N_t_samp1_upr[forecast_rows,] <- NA
    N_t_samp3_upr[forecast_rows,] <- NA
    N_t_samptt_upr[forecast_rows,] <- NA
    # N_t_kern_upr[forecast_rows,] <- NA
    # N_t_simp_upr[forecast_rows,] <- NA

    # remove catch and compute spawning biomass (metric tons)
    N_t_int$B <- rowSums((N_t_int*maturity - caa[,-1]) * mean_wt)
    N_t_bh$B <- rowSums((N_t_bh*maturity - caa[,-1]) * mean_wt)
    N_t_rick$B <- rowSums((N_t_rick*maturity - caa[,-1]) * mean_wt)
    N_t_rick_sst$B <- rowSums((N_t_rick_sst*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_upw$B <- rowSums((N_t_rick_upw*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_soi$B <- rowSums((N_t_rick_soi*maturity - caa[,-1]) * mean_wt)
    N_t_rick_pdo$B <- rowSums((N_t_rick_pdo*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_precip$B <- rowSums((N_t_rick_precip*maturity - caa[,-1]) * mean_wt)
    N_t_samp1$B <- rowSums((N_t_samp1*maturity - caa[,-1]) * mean_wt)
    N_t_samp3$B <- rowSums((N_t_samp3*maturity - caa[,-1]) * mean_wt)
    N_t_samptt$B <- rowSums((N_t_samptt*maturity - caa[,-1]) * mean_wt)
    # N_t_kern$B <- rowSums((N_t_kern*maturity - caa[,-1]) * mean_wt)
    N_t_simp$B <- rowSums((N_t_simp*maturity - caa[,-1]) * mean_wt)

    # for lower confidence bounds
    N_t_int_lwr$B <- rowSums((N_t_int_lwr*maturity - caa[,-1]) * mean_wt) 
    N_t_bh_lwr$B <- rowSums((N_t_bh_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_rick_lwr$B <- rowSums((N_t_rick_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_rick_sst_lwr$B <- rowSums((N_t_rick_sst_lwr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_upw_lwr$B <- rowSums((N_t_rick_upw_lwr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_soi_lwr$B <- rowSums((N_t_rick_soi_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_rick_pdo_lwr$B <- rowSums((N_t_rick_pdo_lwr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_precip_lwr$B <- rowSums((N_t_rick_precip_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_samp1_lwr$B <- rowSums((N_t_samp1_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_samp3_lwr$B <- rowSums((N_t_samp3_lwr*maturity - caa[,-1]) * mean_wt)
    N_t_samptt_lwr$B <- rowSums((N_t_samptt_lwr*maturity - caa[,-1]) * mean_wt)
    # N_t_kern_lwr$B <- rowSums((N_t_kern_lwr*maturity - caa[,-1]) * mean_wt)
    # N_t_simp_lwr$B <- rowSums((N_t_simp*maturity - caa[,-1]) * mean_wt)

    # for upper confidence bounds
    N_t_int_upr$B <- rowSums((N_t_int_upr*maturity - caa[,-1]) * mean_wt)
    N_t_bh_upr$B <- rowSums((N_t_bh_upr*maturity - caa[,-1]) * mean_wt) 
    N_t_rick_upr$B <- rowSums((N_t_rick_upr*maturity - caa[,-1]) * mean_wt)
    N_t_rick_sst_upr$B <- rowSums((N_t_rick_sst_upr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_upw_upr$B <- rowSums((N_t_rick_upw_upr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_soi_upr$B <- rowSums((N_t_rick_soi_upr*maturity - caa[,-1]) * mean_wt)
    N_t_rick_pdo_upr$B <- rowSums((N_t_rick_pdo_upr*maturity - caa[,-1]) * mean_wt)
    # N_t_rick_precip_upr$B <- rowSums((N_t_rick_precip_upr*maturity - caa[,-1]) * mean_wt)
    N_t_samp1_upr$B <- rowSums((N_t_samp1_upr*maturity - caa[,-1]) * mean_wt)
    N_t_samp3_upr$B <- rowSums((N_t_samp3_upr*maturity - caa[,-1]) * mean_wt)
    N_t_samptt_upr$B <- rowSums((N_t_samptt_upr*maturity - caa[,-1]) * mean_wt)
    # N_t_kern_upr$B <- rowSums((N_t_kern_upr*maturity - caa[,-1]) * mean_wt)
    # N_t_simp_upr$B <- rowSums((N_t_simp*maturity - caa[,-1]) * mean_wt)

    #-------------------------------------------------------------------------------------

    ## other things to keep track of...

    # 4-year cycle
    # sr_sim[[l]]$cycle <- factor(rep(c(1,0,0,0), length.out = nrow(sr_sim[[l]])))

    # record which years are real and which are forecasts
    # sr_sim[[l]]$forecast <- factor(0, levels = c(0,1))


    #------------------------------------------------------------------------------------------

    ## for sampling method repetitions

    N <- 1000
    N_t_samp1_reps <- rep(list(N_t_samp1), length = N)
    N_t_samp3_reps <- rep(list(N_t_samp3), length = N)
    N_t_samptt_reps <- rep(list(N_t_samptt), length = N)
    # N_t_kern_reps <- rep(list(N_t_kern), length = N)

    #------------------------------------------------------------------------------------------

    ####################### forecast from current year minus delta ############################

    # this loop produces estimates spawner-recruit estimates for each recruitment forecasting
    # method and iterates over forecast year

    for(t in forecast_rows){

        # this iterates through the spawn years used to fit models for projection
        spawn_year <- 1976:(sr$year[t-1-3]) # 3 for spawn year lag, 1 for forecast

        # collate environmental vars
        sr_vars <- data.table(spawn_year = spawn_year,
                     recruit_year = spawn_year+3,
                     recruits = sr$recruits[sr$year %in% (spawn_year+3)],
                     spawners = sr$spawners[sr$year %in% (spawn_year+3)],
                    #  dec4_precip = precip[Year %in% (spawn_year-1), Dec],
                     sst = sst[Year %in% spawn_year, spring_mean],
                    #  apr3_upw = upw[Year %in% spawn_year, Apr],
                     pdo = pdo[Year %in% spawn_year, spring_mean]
                    #  may0_soi = soi[Year %in% (spawn_year+3), May]
                     )

        tstar <- qt(.975, df = t-2)  # t quantile for 95% confidence bounds

        #--------------- intercept model ---------------------#
        
        inter <- lm(log(recruits) ~ 1, data = sr_vars[-forecast_rows,])
        int_mu <- predict(inter, newdata = data.frame(1))
        int_sigma <- summary(inter)$sigma

        # forecast numbers-at-age with intercept model
        N_t_int[t,] <- data.frame(age3 = exp(int_mu),  
                                age4 = N_t_int[t-1, "age3"] * S,
                                age5 = N_t_int[t-1, "age4"] * S,
                                age6 = N_t_int[t-1, "age5"] * S,
                                age7 = N_t_int[t-1, "age6"] * S,
                                age8 = sum(N_t_int[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # repeat for lower and upper confidence bounds
        N_t_int_lwr[t,] <- data.frame(age3 = exp(int_mu - (tstar*(int_sigma/sqrt(t-1))) ),  
                                age4 = N_t_int_lwr[t-1, "age3"] * S,
                                age5 = N_t_int_lwr[t-1, "age4"] * S,
                                age6 = N_t_int_lwr[t-1, "age5"] * S,
                                age7 = N_t_int_lwr[t-1, "age6"] * S,
                                age8 = sum(N_t_int_lwr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )
        N_t_int_upr[t,] <- data.frame(age3 = exp(int_mu + (tstar*(int_sigma/sqrt(t-1)))),  
                                age4 = N_t_int_upr[t-1, "age3"] * S,
                                age5 = N_t_int_upr[t-1, "age4"] * S,
                                age6 = N_t_int_upr[t-1, "age5"] * S,
                                age7 = N_t_int_upr[t-1, "age6"] * S,
                                age8 = sum(N_t_int_upr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast spawning biomass (metric tons)
        N_t_int[t,"B"] <- rowSums((N_t_int[t, !names(N_t_int) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 
        N_t_int_lwr[t,"B"] <- rowSums((N_t_int_lwr[t, !names(N_t_int_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 
        N_t_int_upr[t,"B"] <- rowSums((N_t_int_upr[t, !names(N_t_int_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 

        #--------------------- beverton-holt -------------------------#

        # fit model
        # minuslogl <- function(logalpha, logbeta, logsigma){
        #                 alpha <- exp(logalpha)
        #                 beta <- exp(logbeta)
        #                 sigma <- exp(logsigma)

        #                 r <- unlist(sr_vars[-forecast_rows, "recruits"])
        #                 s <- unlist(sr_vars[-forecast_rows, "spawners"])

        #                 logpreds <- log(s / (alpha + (beta*s)))

        #                 minuslogl <- -1*sum(dnorm(log(r), mean = logpreds,
        #                                             sd = sigma, log = TRUE))
        #                 return(minuslogl)
        #             }
        # mleFit <- stats4::mle(minuslogl = minuslogl, start = list(logalpha = 2, logbeta = 3, logsigma = 2))
        # mod <- function(x){
        #     alpha <- exp(coef(mleFit)[1])
        #     beta <- exp(coef(mleFit)[2])
        #     out <- x / (alpha + (beta*x))
        #     return(out)
        # }

        bh <- fit_bevertonHolt(recruits = unlist(sr_vars[-forecast_rows,"recruits"]), 
                               spawners = unlist(sr_vars[-forecast_rows,"spawners"]), 
                               start = list(logalpha = 2, logbeta = 3, logsigma2 = 2), 
                               errorStructure = "multiplicative")
        bh_mu <- log(bh$mod(N_t_bh$B[t-3]))
        bh_sigma <- sqrt(bh$coef["sigma2"])  

        # forecast numbers at age with beverton-holt model  
        N_t_bh[t,] <- data.frame(age3 = exp(bh_mu),
                                age4 = N_t_bh[t-1, "age3"] * S,
                                age5 = N_t_bh[t-1, "age4"] * S,
                                age6 = N_t_bh[t-1, "age5"] * S,
                                age7 = N_t_bh[t-1, "age6"] * S,
                                age8 = sum(N_t_bh[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # repeat for lower and upper confidence bounds
        N_t_bh_lwr[t,] <- data.frame(age3 = exp(bh_mu - (tstar*(bh_sigma/sqrt(t-1)))),
                                age4 = N_t_bh_lwr[t-1, "age3"] * S,
                                age5 = N_t_bh_lwr[t-1, "age4"] * S,
                                age6 = N_t_bh_lwr[t-1, "age5"] * S,
                                age7 = N_t_bh_lwr[t-1, "age6"] * S,
                                age8 = sum(N_t_bh_lwr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )
        N_t_bh_upr[t,] <- data.frame(age3 = exp(bh_mu  + (tstar*(bh_sigma/sqrt(t-1)))),
                                age4 = N_t_bh_upr[t-1, "age3"] * S,
                                age5 = N_t_bh_upr[t-1, "age4"] * S,
                                age6 = N_t_bh_upr[t-1, "age5"] * S,
                                age7 = N_t_bh_upr[t-1, "age6"] * S,
                                age8 = sum(N_t_bh_upr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast biomass
        N_t_bh[t,"B"] <- rowSums((N_t_bh[t, !names(N_t_bh) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_bh_lwr[t,"B"] <- rowSums((N_t_bh_lwr[t, !names(N_t_bh_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_bh_upr[t,"B"] <- rowSums((N_t_bh_upr[t, !names(N_t_bh_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 
        
        #--------------------- ricker -------------------------#

        # fit model
        ricker <- lm(log(recruits/spawners) ~ 1 + spawners, data = sr_vars[-forecast_rows,])
        rick_mu <- predict(ricker, newdata = list(spawners = N_t_rick$B[t-3]))
        rick_sigma <- summary(ricker)$sigma     

        # forecast numbers at age with ricker model  
        N_t_rick[t,] <- data.frame(age3 = exp(rick_mu) * N_t_rick$B[t-3],
                                age4 = N_t_rick[t-1, "age3"] * S,
                                age5 = N_t_rick[t-1, "age4"] * S,
                                age6 = N_t_rick[t-1, "age5"] * S,
                                age7 = N_t_rick[t-1, "age6"] * S,
                                age8 = sum(N_t_rick[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # repeat for lower and upper confidence bounds
        N_t_rick_lwr[t,] <- data.frame(age3 = exp(rick_mu - (tstar*(rick_sigma/sqrt(t-1))))*N_t_rick$B[t-3],
                                age4 = N_t_rick_lwr[t-1, "age3"] * S,
                                age5 = N_t_rick_lwr[t-1, "age4"] * S,
                                age6 = N_t_rick_lwr[t-1, "age5"] * S,
                                age7 = N_t_rick_lwr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_lwr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )
        N_t_rick_upr[t,] <- data.frame(age3 = exp(rick_mu  + (tstar*(rick_sigma/sqrt(t-1))))*N_t_rick$B[t-3],
                                age4 = N_t_rick_upr[t-1, "age3"] * S,
                                age5 = N_t_rick_upr[t-1, "age4"] * S,
                                age6 = N_t_rick_upr[t-1, "age5"] * S,
                                age7 = N_t_rick_upr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_upr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast biomass
        N_t_rick[t,"B"] <- rowSums((N_t_rick[t, !names(N_t_rick) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_lwr[t,"B"] <- rowSums((N_t_rick_lwr[t, !names(N_t_rick_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_upr[t,"B"] <- rowSums((N_t_rick_upr[t, !names(N_t_rick_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 

        #--------------------- ricker with sst -------------------------#

        # fit model
        ricker_sst <- lm(log(recruits/spawners) ~ 1 + spawners + sst, data = sr_vars[-forecast_rows,])
        rick_sst_mu <- predict(ricker_sst, newdata = list(spawners = N_t_rick_sst$B[t-3],
                                                      sst = sst$spring_mean[t]))
        rick_sst_sigma <- summary(ricker_sst)$sigma     

        # forecast numbers at age with ricker model  
        N_t_rick_sst[t,] <- data.frame(age3 = exp(rick_sst_mu) * N_t_rick_sst$B[t-3],
                                age4 = N_t_rick_sst[t-1, "age3"] * S,
                                age5 = N_t_rick_sst[t-1, "age4"] * S,
                                age6 = N_t_rick_sst[t-1, "age5"] * S,
                                age7 = N_t_rick_sst[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_sst[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # repeat for lower and upper confidence bounds
        N_t_rick_sst_lwr[t,] <- data.frame(age3 = exp(rick_sst_mu - 
                                    (tstar*(rick_sst_sigma/sqrt(t-1))))*N_t_rick_sst$B[t-3],
                                age4 = N_t_rick_sst_lwr[t-1, "age3"] * S,
                                age5 = N_t_rick_sst_lwr[t-1, "age4"] * S,
                                age6 = N_t_rick_sst_lwr[t-1, "age5"] * S,
                                age7 = N_t_rick_sst_lwr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_sst_lwr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )
        N_t_rick_sst_upr[t,] <- data.frame(age3 = exp(rick_sst_mu  + 
                                    (tstar*(rick_sst_sigma/sqrt(t-1))))*N_t_rick_sst$B[t-3],
                                age4 = N_t_rick_sst_upr[t-1, "age3"] * S,
                                age5 = N_t_rick_sst_upr[t-1, "age4"] * S,
                                age6 = N_t_rick_sst_upr[t-1, "age5"] * S,
                                age7 = N_t_rick_sst_upr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_sst_upr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast biomass
        N_t_rick_sst[t,"B"] <- rowSums((N_t_rick_sst[t, !names(N_t_rick_sst) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_sst_lwr[t,"B"] <- rowSums((N_t_rick_sst_lwr[t, !names(N_t_rick_sst_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_sst_upr[t,"B"] <- rowSums((N_t_rick_sst_upr[t, !names(N_t_rick_sst_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 

        # #--------------------- ricker with upw -------------------------#

        # # fit model
        # ricker_upw <- lm(log(recruits/spawners) ~ 1 + spawners + apr3_upw, data = sr_vars[-forecast_rows,])
        # rick_upw_mu <- predict(ricker_upw, newdata = list(spawners = N_t_rick_upw$B[t-3],
        #                                               apr3_upw = upw$Apr[t]))
        # rick_upw_sigma <- summary(ricker_upw)$sigma     

        # # forecast numbers at age with ricker model  
        # N_t_rick_upw[t,] <- data.frame(age3 = exp(rick_upw_mu) * N_t_rick_upw$B[t-3],
        #                         age4 = N_t_rick_upw[t-1, "age3"] * S,
        #                         age5 = N_t_rick_upw[t-1, "age4"] * S,
        #                         age6 = N_t_rick_upw[t-1, "age5"] * S,
        #                         age7 = N_t_rick_upw[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_upw[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # repeat for lower and upper confidence bounds
        # N_t_rick_upw_lwr[t,] <- data.frame(age3 = exp(rick_upw_mu - 
        #                             (tstar*(rick_upw_sigma/sqrt(t-1))))*N_t_rick_upw$B[t-3],
        #                         age4 = N_t_rick_upw_lwr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_upw_lwr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_upw_lwr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_upw_lwr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_upw_lwr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )
        # N_t_rick_upw_upr[t,] <- data.frame(age3 = exp(rick_upw_mu  + 
        #                             (tstar*(rick_upw_sigma/sqrt(t-1))))*N_t_rick_upw$B[t-3],
        #                         age4 = N_t_rick_upw_upr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_upw_upr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_upw_upr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_upw_upr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_upw_upr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # remove catch and forecast biomass
        # N_t_rick_upw[t,"B"] <- rowSums((N_t_rick_upw[t, !names(N_t_rick_upw) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_upw_lwr[t,"B"] <- rowSums((N_t_rick_upw_lwr[t, !names(N_t_rick_upw_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_upw_upr[t,"B"] <- rowSums((N_t_rick_upw_upr[t, !names(N_t_rick_upw_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 


        # #--------------------- ricker with soi -------------------------#

        # # fit model
        # ricker_soi <- lm(log(recruits/spawners) ~ 1 + spawners + may0_soi, data = sr_vars[-forecast_rows,])
        # rick_soi_mu <- predict(ricker_soi, newdata = list(spawners = N_t_rick_soi$B[t-3],
        #                                               may0_soi = soi$May[t]))
        # rick_soi_sigma <- summary(ricker_soi)$sigma     

        # # forecast numbers at age with ricker model  
        # N_t_rick_soi[t,] <- data.frame(age3 = exp(rick_soi_mu) * N_t_rick_soi$B[t-3],
        #                         age4 = N_t_rick_soi[t-1, "age3"] * S,
        #                         age5 = N_t_rick_soi[t-1, "age4"] * S,
        #                         age6 = N_t_rick_soi[t-1, "age5"] * S,
        #                         age7 = N_t_rick_soi[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_soi[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # repeat for lower and upper confidence bounds
        # N_t_rick_soi_lwr[t,] <- data.frame(age3 = exp(rick_soi_mu - 
        #                             (tstar*(rick_soi_sigma/sqrt(t-1))))*N_t_rick_soi$B[t-3],
        #                         age4 = N_t_rick_soi_lwr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_soi_lwr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_soi_lwr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_soi_lwr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_soi_lwr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )
        # N_t_rick_soi_upr[t,] <- data.frame(age3 = exp(rick_soi_mu  + 
        #                             (tstar*(rick_soi_sigma/sqrt(t-1))))*N_t_rick_soi$B[t-3],
        #                         age4 = N_t_rick_soi_upr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_soi_upr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_soi_upr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_soi_upr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_soi_upr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # remove catch and forecast biomass
        # N_t_rick_soi[t,"B"] <- rowSums((N_t_rick_soi[t, !names(N_t_rick_soi) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_soi_lwr[t,"B"] <- rowSums((N_t_rick_soi_lwr[t, !names(N_t_rick_soi_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_soi_upr[t,"B"] <- rowSums((N_t_rick_soi_upr[t, !names(N_t_rick_soi_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 


        #--------------------- ricker with pdo -------------------------#

        # fit model
        ricker_pdo <- lm(log(recruits/spawners) ~ 1 + spawners + pdo, data = sr_vars[-forecast_rows,])
        rick_pdo_mu <- predict(ricker_pdo, newdata = list(spawners = N_t_rick_pdo$B[t-3],
                                                      pdo = pdo$spring_mean[t]))
        rick_pdo_sigma <- summary(ricker_pdo)$sigma     

        # forecast numbers at age with ricker model  
        N_t_rick_pdo[t,] <- data.frame(age3 = exp(rick_pdo_mu) * N_t_rick_pdo$B[t-3],
                                age4 = N_t_rick_pdo[t-1, "age3"] * S,
                                age5 = N_t_rick_pdo[t-1, "age4"] * S,
                                age6 = N_t_rick_pdo[t-1, "age5"] * S,
                                age7 = N_t_rick_pdo[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_pdo[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # repeat for lower and upper confidence bounds
        N_t_rick_pdo_lwr[t,] <- data.frame(age3 = exp(rick_pdo_mu - 
                                    (tstar*(rick_pdo_sigma/sqrt(t-1))))*N_t_rick_pdo$B[t-3],
                                age4 = N_t_rick_pdo_lwr[t-1, "age3"] * S,
                                age5 = N_t_rick_pdo_lwr[t-1, "age4"] * S,
                                age6 = N_t_rick_pdo_lwr[t-1, "age5"] * S,
                                age7 = N_t_rick_pdo_lwr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_pdo_lwr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )
        N_t_rick_pdo_upr[t,] <- data.frame(age3 = exp(rick_pdo_mu  + 
                                    (tstar*(rick_pdo_sigma/sqrt(t-1))))*N_t_rick_pdo$B[t-3],
                                age4 = N_t_rick_pdo_upr[t-1, "age3"] * S,
                                age5 = N_t_rick_pdo_upr[t-1, "age4"] * S,
                                age6 = N_t_rick_pdo_upr[t-1, "age5"] * S,
                                age7 = N_t_rick_pdo_upr[t-1, "age6"] * S,
                                age8 = sum(N_t_rick_pdo_upr[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast biomass
        N_t_rick_pdo[t,"B"] <- rowSums((N_t_rick_pdo[t, !names(N_t_rick_pdo) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_pdo_lwr[t,"B"] <- rowSums((N_t_rick_pdo_lwr[t, !names(N_t_rick_pdo_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        N_t_rick_pdo_upr[t,"B"] <- rowSums((N_t_rick_pdo_upr[t, !names(N_t_rick_pdo_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 


        # #--------------------- ricker with precip -------------------------#

        # # fit model
        # ricker_precip <- lm(log(recruits/spawners) ~ 1 + spawners + dec4_precip, data = sr_vars[-forecast_rows,])
        # rick_precip_mu <- predict(ricker_precip, newdata = list(spawners = N_t_rick_precip$B[t-3],
        #                                               dec4_precip = precip$Dec[t]))
        # rick_precip_sigma <- summary(ricker_precip)$sigma     

        # # forecast numbers at age with ricker model  
        # N_t_rick_precip[t,] <- data.frame(age3 = exp(rick_precip_mu) * N_t_rick_precip$B[t-3],
        #                         age4 = N_t_rick_precip[t-1, "age3"] * S,
        #                         age5 = N_t_rick_precip[t-1, "age4"] * S,
        #                         age6 = N_t_rick_precip[t-1, "age5"] * S,
        #                         age7 = N_t_rick_precip[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_precip[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # repeat for lower and upper confidence bounds
        # N_t_rick_precip_lwr[t,] <- data.frame(age3 = exp(rick_precip_mu - 
        #                             (tstar*(rick_precip_sigma/sqrt(t-1))))*N_t_rick_precip$B[t-3],
        #                         age4 = N_t_rick_precip_lwr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_precip_lwr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_precip_lwr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_precip_lwr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_precip_lwr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )
        # N_t_rick_precip_upr[t,] <- data.frame(age3 = exp(rick_precip_mu  + 
        #                             (tstar*(rick_precip_sigma/sqrt(t-1))))*N_t_rick_precip$B[t-3],
        #                         age4 = N_t_rick_precip_upr[t-1, "age3"] * S,
        #                         age5 = N_t_rick_precip_upr[t-1, "age4"] * S,
        #                         age6 = N_t_rick_precip_upr[t-1, "age5"] * S,
        #                         age7 = N_t_rick_precip_upr[t-1, "age6"] * S,
        #                         age8 = sum(N_t_rick_precip_upr[t-1, c("age7", "age8")]*S),  # plus group
        #                         B = NA
        #                         )

        # # remove catch and forecast biomass
        # N_t_rick_precip[t,"B"] <- rowSums((N_t_rick_precip[t, !names(N_t_rick_precip) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_precip_lwr[t,"B"] <- rowSums((N_t_rick_precip_lwr[t, !names(N_t_rick_precip_lwr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        # N_t_rick_precip_upr[t,"B"] <- rowSums((N_t_rick_precip_upr[t, !names(N_t_rick_precip_upr) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,]) 

        #---------------- empirical model - 1 strata -------------------#

        # repeat recruit sampling procedure/biomass calculation N times to develop variance structure
        for(j in 1:N){
        # forecast numbers at age by sampling recruits in training years 
        N_t_samp1_reps[[j]][t,] <- data.frame(age3 = sample(N_t_samp1_reps[[j]][1:(t-1),"age3"], size = 1),
                                    age4 = N_t_samp1_reps[[j]][t-1, "age3"] * S,
                                    age5 = N_t_samp1_reps[[j]][t-1, "age4"] * S,
                                    age6 = N_t_samp1_reps[[j]][t-1, "age5"] * S,
                                    age7 = N_t_samp1_reps[[j]][t-1, "age6"] * S,
                                    age8 = sum(N_t_samp1_reps[[j]][t-1, c("age7", "age8")]*S),  # plus group
                                    B = NA
                                    )
        # remove catch and forecast biomass
        N_t_samp1_reps[[j]][t,"B"] <- rowSums((N_t_samp1_reps[[j]][t, !names(N_t_samp1_reps[[j]]) %in% "B"] * 
                                                    maturity[t,] - caa[t,-1]) * mean_wt[t,])    
        }

        # repetitions-at-age for year t
        raa_samp1 <- do.call("rbind", lapply(N_t_samp1_reps, FUN = function(x) x[t,]))

        # median biomass-at-age
        N_t_samp1[t,] <- c(lapply(raa_samp1[, !names(raa_samp1) %in% "B"], FUN = median), median(raa_samp1$B))

        # repeat for lower and upper confidence bounds
        N_t_samp1_lwr[t,] <- c(lapply(raa_samp1[, !names(raa_samp1) %in% "B"], FUN = function(x) quantile(x, .025)), 
                        quantile(raa_samp1$B, .025))
        N_t_samp1_upr[t,] <- c(lapply(raa_samp1[, !names(raa_samp1) %in% "B"], FUN = function(x) quantile(x, .975)), 
                        quantile(raa_samp1$B, .975))

        #---------------- empirical model - 3 strata -------------------#

        # # to cluster spawners and recruits up until year t
        # tmp_recruits <- na.omit(N_t_samp3[1:(t-1),"age3"])
        # tmp_spawners <- na.omit(c(sr_sim[[l]]$spawners_samp3[1:3], N_t_samp3[1:(t-4),"B"]))

        # repeat recruit sampling procedure/biomass calculation N times to develop variance structure
        for(j in 1:N){

            # run k-means to obtain sampling strata for each repetition 
            strata <- factor(kmeans(cbind(sr$recruits, sr$spawners), centers=3)$cluster)
            sr_sim_list <- split_sr(sr$recruits, sr$spawners, strata)
            splits <- strata_boundaries(sr_sim_list)

            if(N_t_samp3_reps[[j]][t-3,"B"] < splits[1]){
                samp3_recruit <- sr_sim_list[[1]][sample(x = 1:nrow(sr_sim_list[[1]]), 1), "recruits"]
            } else if((splits[1] <= N_t_samp3_reps[[j]][t-3,"B"]) & (N_t_samp3_reps[[j]][t-3,"B"] < splits[2])){
                samp3_recruit <- sr_sim_list[[2]][sample(x = 1:nrow(sr_sim_list[[2]]), 1), "recruits"]
            } else if(splits[2] <= N_t_samp3_reps[[j]][t-3,"B"]){
                samp3_recruit <- sr_sim_list[[3]][sample(x = 1:nrow(sr_sim_list[[3]]), 1), "recruits"]
            }

            # forecast numbers at age by sampling stratified recruits in training years 
            N_t_samp3_reps[[j]][t,] <- data.frame(age3 = samp3_recruit,
                                                age4 = N_t_samp3[t-1, "age3"] * S,
                                                age5 = N_t_samp3[t-1, "age4"] * S,
                                                age6 = N_t_samp3[t-1, "age5"] * S,
                                                age7 = N_t_samp3[t-1, "age6"] * S,
                                                age8 = sum(N_t_samp3[t-1, c("age7", "age8")]*S),  # plus group
                                                B = NA
                                                ) 
            # remove catch and forecast biomass
            N_t_samp3_reps[[j]][t,"B"] <- rowSums((N_t_samp3_reps[[j]][t, !names(N_t_samp3_reps[[j]]) %in% "B"] * 
                                                            maturity[t,] - caa[t,-1]) * mean_wt[t,])
        }

        # repetitions-at-age for year t
        raa_samp3 <- do.call("rbind", lapply(N_t_samp3_reps, FUN = function(x) x[t,]))

        # median biomass-at-age
        N_t_samp3[t,] <- c(lapply(raa_samp3[, !names(raa_samp3) %in% "B"], FUN = median), median(raa_samp3$B))

        # repeat for lower and upper confidence bounds
        N_t_samp3_lwr[t,] <- c(lapply(raa_samp3[, !names(raa_samp3) %in% "B"], FUN = function(x) quantile(x, .025)), 
                        quantile(raa_samp3$B, .025))
        N_t_samp3_upr[t,] <- c(lapply(raa_samp3[, !names(raa_samp3) %in% "B"], FUN = function(x) quantile(x, .975)), 
                        quantile(raa_samp3$B, .975))

        #---------------- time-tapered sampling -------------------#

        tt_weights <- seq(sr_sim[[l]]$year)/length(sr_sim[[l]]$year)

        # repeat recruit sampling procedure/biomass calculation N times to develop variance structure
        for(j in 1:N){
        # forecast numbers at age by sampling recruits in training years 
        N_t_samptt_reps[[j]][t,] <- data.frame(age3 = sample(N_t_samptt_reps[[j]][1:(t-1),"age3"], size = 1, prob = tt_weights),
                                    age4 = N_t_samptt_reps[[j]][t-1, "age3"] * S,
                                    age5 = N_t_samptt_reps[[j]][t-1, "age4"] * S,
                                    age6 = N_t_samptt_reps[[j]][t-1, "age5"] * S,
                                    age7 = N_t_samptt_reps[[j]][t-1, "age6"] * S,
                                    age8 = sum(N_t_samptt_reps[[j]][t-1, c("age7", "age8")]*S),  # plus group
                                    B = NA
                                    )
        # remove catch and forecast biomass
        N_t_samptt_reps[[j]][t,"B"] <- rowSums((N_t_samptt_reps[[j]][t, !names(N_t_samptt_reps[[j]]) %in% "B"] * 
                                                    maturity[t,] - caa[t,-1]) * mean_wt[t,])    
        }

        # repetitions-at-age for year t
        raa_samptt <- do.call("rbind", lapply(N_t_samptt_reps, FUN = function(x) x[t,]))

        # median biomass-at-age
        N_t_samptt[t,] <- c(lapply(raa_samptt[, !names(raa_samptt) %in% "B"], FUN = median), median(raa_samptt$B))

        # repeat for lower and upper confidence bounds
        N_t_samptt_lwr[t,] <- c(lapply(raa_samptt[, !names(raa_samptt) %in% "B"], FUN = function(x) quantile(x, .025)), 
                        quantile(raa_samptt$B, .025))
        N_t_samptt_upr[t,] <- c(lapply(raa_samptt[, !names(raa_samptt) %in% "B"], FUN = function(x) quantile(x, .975)), 
                        quantile(raa_samptt$B, .975))

        # #----------------- sample kernel density ---------------------#

        # kern_dens <- density(sr_sim[[l]]$recruits_kern[-forecast_rows], from = 0)
        
        # # take N samples from kernel density for variance estimation
        # for(j in 1:N){
        #     recruits_kern <- sample(kern_dens$x, size = 1, prob = kern_dens$y)

        #     # forecast numbers at age by kernel density in training years 
        #     N_t_kern_reps[[j]][t,] <- data.frame(age3 = recruits_kern,
        #                                 age4 = N_t_kern_reps[[j]][t-1, "age3"] * S,
        #                                 age5 = N_t_kern_reps[[j]][t-1, "age4"] * S,
        #                                 age6 = N_t_kern_reps[[j]][t-1, "age5"] * S,
        #                                 age7 = N_t_kern_reps[[j]][t-1, "age6"] * S,
        #                                 age8 = sum(N_t_kern_reps[[j]][t-1, c("age7", "age8")]*S),  # plus group
        #                                 B = NA
        #                                 )

        #     # remove catch and forecast biomass
        #     N_t_kern_reps[[j]][t,"B"] <- rowSums((N_t_kern_reps[[j]][t, !names(N_t_kern_reps[[j]]) %in% "B"]
        #                                             * maturity[t,] - caa[t,-1]) * mean_wt[t,]) 

        # }

        # # repetitions-at-age for year t
        # raa_kern <- do.call("rbind", lapply(N_t_kern_reps, FUN = function(x) x[t,]))

        # # median biomass-at-age
        # N_t_kern[t,] <- c(lapply(raa_kern[, !names(raa_kern) %in% "B"], FUN = median), median(raa_kern$B))

        # # repeat for lower and upper confidence bounds
        # N_t_kern_lwr[t,] <- c(lapply(raa_kern[, !names(raa_kern) %in% "B"], FUN = function(x) quantile(x, .025)), 
        #                 quantile(raa_kern$B, .025))
        # N_t_kern_upr[t,] <- c(lapply(raa_kern[, !names(raa_kern) %in% "B"], FUN = function(x) quantile(x, .975)), 
        #                 quantile(raa_kern$B, .975))

        #------------------- simplex forecasting ---------------------#

        embed_recruits <- Embed(sr_sim[[l]]$recruits_simp, dimension = E)
        dists <- as.matrix(dist(embed_recruits))[nrow(sr_sim[[l]])-E+1,]
        d <- dists[!dists %in% 0]

        segment_projections <- sr_sim[[l]]$recruits_simp[order(d)+E][1:k]
        weights <- exp(-d[order(d)] / min(d))[1:k]

        # forecast numbers-at-age with simplex
        simp_forecast <- sum(segment_projections*weights) / sum(weights)

        N_t_simp[t,] <- data.frame(age3 = simp_forecast,
                                age4 = N_t_simp[t-1, "age3"] * S,
                                age5 = N_t_simp[t-1, "age4"] * S,
                                age6 = N_t_simp[t-1, "age5"] * S,
                                age7 = N_t_simp[t-1, "age6"] * S,
                                age8 = sum(N_t_simp[t-1, c("age7", "age8")]*S),  # plus group
                                B = NA
                                )

        # remove catch and forecast biomass
        N_t_simp[t,"B"] <- rowSums((N_t_simp[t, !names(N_t_simp) %in% "B"]*maturity[t,] - caa[t,-1]) * mean_wt[t,])
        

        #--------------- update sr_sim with results ------------------#
        sr_sim[[l]][t,] <- data.frame(year = min(recruit_year) - 1 + t,
                                spawners_int = N_t_int[t-3,"B"],
                                recruits_int = N_t_int[t,"age3"],
                                spawners_bh = N_t_bh[t-3,"B"],
                                recruits_bh = N_t_bh[t,"age3"],
                                spawners_rick = N_t_rick[t-3,"B"],
                                recruits_rick = N_t_rick[t,"age3"],
                                spawners_rick_sst = N_t_rick_sst[t-3,"B"],
                                recruits_rick_sst = N_t_rick_sst[t,"age3"],
                                # spawners_rick_upw = N_t_rick_upw[t-3,"B"],
                                # recruits_rick_upw = N_t_rick_upw[t,"age3"],
                                # spawners_rick_soi = N_t_rick_soi[t-3,"B"],
                                # recruits_rick_soi = N_t_rick_soi[t,"age3"],
                                spawners_rick_pdo = N_t_rick_pdo[t-3,"B"],
                                recruits_rick_pdo = N_t_rick_pdo[t,"age3"],
                                # spawners_rick_precip = N_t_rick_precip[t-3,"B"],
                                # recruits_rick_precip = N_t_rick_precip[t,"age3"],
                                spawners_samp1 = N_t_samp1[t-3,"B"],
                                recruits_samp1 = N_t_samp1[t,"age3"],
                                spawners_samp3 = N_t_samp3[t-3,"B"],
                                recruits_samp3 = N_t_samp3[t,"age3"],
                                spawners_samptt = N_t_samptt[t-3,"B"],
                                recruits_samptt = N_t_samptt[t,"age3"],
                                # spawners_kern = N_t_kern[t-3,"B"],
                                # recruits_kern = N_t_kern[t,"age3"],
                                spawners_simp = N_t_simp[t-3,"B"],
                                recruits_simp = N_t_simp[t,"age3"],
                                int_lwr = N_t_int_lwr[t-3,"B"],
                                int_upr = N_t_int_upr[t-3,"B"],
                                bh_lwr = N_t_bh_lwr[t-3,"B"],
                                bh_upr = N_t_bh_upr[t-3,"B"],
                                rick_lwr = N_t_rick_lwr[t-3,"B"],
                                rick_upr = N_t_rick_upr[t-3,"B"],
                                rick_sst_lwr = N_t_rick_sst_lwr[t-3,"B"],
                                rick_sst_upr = N_t_rick_sst_upr[t-3,"B"],
                                # rick_upw_lwr = N_t_rick_upw_lwr[t-3,"B"],
                                # rick_upw_upr = N_t_rick_upw_upr[t-3,"B"],
                                # rick_soi_lwr = N_t_rick_soi_lwr[t-3,"B"],
                                # rick_soi_upr = N_t_rick_soi_upr[t-3,"B"],
                                rick_pdo_lwr = N_t_rick_pdo_lwr[t-3,"B"],
                                rick_pdo_upr = N_t_rick_pdo_upr[t-3,"B"],
                                # rick_precip_lwr = N_t_rick_precip_lwr[t-3,"B"],
                                # rick_precip_upr = N_t_rick_precip_upr[t-3,"B"],
                                samp1_lwr = N_t_samp1_lwr[t-3,"B"],
                                samp1_upr = N_t_samp1_upr[t-3,"B"],
                                samp3_lwr = N_t_samp3_lwr[t-3,"B"],
                                samp3_upr = N_t_samp3_upr[t-3,"B"],
                                samptt_lwr = N_t_samptt_lwr[t-3,"B"],
                                samptt_upr = N_t_samptt_upr[t-3,"B"],
                                # kern_lwr = N_t_kern_lwr[t-3,"B"],
                                # kern_upr = N_t_kern_upr[t-3,"B"],
                                simp_lwr = N_t_simp[t-3,"B"],
                                simp_upr = N_t_simp[t-3,"B"]
                                )
    }
    # replaces negative recruit forecasts with trivially small positive number
    sr_sim[[l]] <- replace(sr_sim[[l]], list = sr_sim[[l]] < 0, values = 1e-5)  
    sr_sim[[l]]$recruits_true <- sr$recruits_true
    sr_sim[[l]]$spawners_true <- sr$spawners_true
}



# save results

saveRDS(sr_sim, file = "recruitment_comparisons/data/sr_sim_1979-2022.rds")

end_time <- Sys.time()
print(end_time - start_time)