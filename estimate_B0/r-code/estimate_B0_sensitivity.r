################################################################################

# Sitka herring R average unfished biomass simulation

# this script estimates the unfished biomass of Sitka Sound herring by simulating
# the stock biomass trajectory over many years. This simulation is repeated many
# times, and all simulated years of biomass are combined in a single nonparametric
# distribution. The median of this distribution is taken to be the estimate for
# unfished biomass of Sitka Sound herring. 

# Sensitivity analysis: large recruitment in 2019 omitted to see how results change

# Inputs (read in via herringSim, time series 1980-2022):
#   - mean weight-at-age from purse-seine fishery
#   - SCAA-estimated age-specific maturity 
#   - SCAA-estimated time-block survival
#   - SCAA-estimated spawning biomass
#   - SCAA-estimated age-3 recruits

# Outputs: 
#   - data/final_strata_sensitivity.rda: the sampling strata as identified by kmeans
#   - data/raw_results_many_simulations_asa2023_sensitivity.txt: huge file containing 
#     combined simulation results
#   - plots/combined_histogram.png: plot of combined results with median

# estimated runtime: 8 hours

# By: CL Roberts

################################################################################

#### front matter ####

## attach packages

library(herringSim)
library(dplyr)
library(ggplot2)

## load data

sr <- sr_asa2023_forecast |>
    filter((year >= 1980) & (year != 2019))
mean_wt <- mean_wt_asa2023_forecast |>
    mutate(year = rownames(mean_wt_asa2023_forecast)) |>
    filter((year >= 1980) & (year != 2019)) |>
    mutate(year = NULL) |>
    colMeans()
maturity <- maturity_asa2023_forecast |>
    mutate(year = rownames(maturity_asa2023_forecast)) |>
    filter((year >= 1980) & (year != 2019)) |>
    mutate(year = NULL) |>
    colMeans()
S <- S_asa2023_forecast |>
    mutate(year = rownames(S_asa2023_forecast)) |>
    filter((year >= 1980) & (year != 2019)) |>
    mutate(year = NULL) |> 
    colMeans() |>
    unique()

## set global variables

seed <- 123
num_years <- 30000
N <- 1000
data("cols")


#---------------------------------------------------------------------------------

#### determine strata boundaries ####

## this bit repeats kmeans clustering many times. The modal clusters are taken to be the simulation strata

set.seed(seed)
repeated_clusters <- as.data.frame(matrix(NA, ncol = nrow(sr), nrow = 100, dimnames = list(NULL, sr$year)))
for(i in 1:100){
  kmeans_3clusters <- kmeans(as.matrix(sr[,c("spawners", "recruits")]), centers = 3)
  clust_order <- order(kmeans_3clusters$centers[,"spawners"])
  strata_ordered <- c()
  for(l in 1:nrow(sr)){
    if(kmeans_3clusters$cluster[l] == clust_order[1]){
      strata_ordered[l] <- 1
    } else if(kmeans_3clusters$cluster[l] == clust_order[2]){
      strata_ordered[l] <- 2
    } else if(kmeans_3clusters$cluster[l] == clust_order[3]){
      strata_ordered[l] <- 3
    } 
  }
  repeated_clusters[i,] <- strata_ordered
}

# final strata

sr$strata <- apply(repeated_clusters, MARGIN = 2, FUN = Mode)
saveRDS(sr, file = "estimate_B0/data/final_strata_sensitivity.rda")

sr_list <- split_sr(sr$recruits, sr$spawners, sr$strata)
splits <- strata_boundaries(sr_list)
plot_strata(sr$recruits, sr$spawners, as.factor(sr$strata), splits)

#---------------------------------------------------------------------------------

#### many simulations ####

## repeat simulation N times, saving each threshold/AUB along the way

# aubs <- c()
# set.seed(seed)
# for(i in 1:N){
#   B <- B_sim_empirical(recruits = sr$recruits, spawners = sr$spawners,
#                            strata = sr$strata, mean_wt = mean_wt, maturity = maturity,
#                            S = S, num_years = num_years, max_iters = 40000)
#   B$iteration <- i
#   if (i == 1) {
#     raw_results <- B
#   } else if (i > 1) {
#     raw_results <- rbind(raw_results, B)
#   }  
#   print(head(B))
#   print(paste0("AUB in simulation year ", i, " is ", aubs[i], " tons"))
#   print(paste0("Simulation is ", 100*(i/N), "% complete"))
# }

## write data

# write.table(raw_results, "estimate_B0/data/raw_results_many_simulations_asa2023_sensitivity.txt", row.names = FALSE, col.names = TRUE)

#---------------------------------------------------------------------------------

#### plot results ####

## combined histogram of all simulated biomasses

raw_results <- data.table::fread("estimate_B0/data/raw_results_many_simulations_asa2023_sensitivity.txt", header = TRUE)

raw_results_post_convergence <- na.omit(raw_results)

quants_combined <- quantile(raw_results_post_convergence$B, probs = c(.025, .25, .5, .75, .975))

combined_hist <- ggplot() +
  geom_polygon(aes(x = c(quants_combined[1], quants_combined[5], quants_combined[5], quants_combined[1]),
                   y = c(0, 0, .1, .1)),
               fill = "grey50", alpha = .5) +
  geom_polygon(aes(x = c(quants_combined[2], quants_combined[4], quants_combined[4], quants_combined[2]),
                   y = c(0, 0, .1, .1)),
               fill = "grey50", alpha = .5) +
  geom_histogram(data = raw_results_post_convergence, aes(x = B, y = after_stat(count/sum(count))),
                 binwidth = 5000, color = "black", fill = "grey50", alpha = .5) +
  geom_vline(aes(xintercept = quants_combined[3]), color = cols[2]) +  
  xlab("Spawning Biomass (tons)") +
  ylab("Proportion of All Simulated Years (%)") +
  labs(title = "Combined Distribution of All Biomass Simulations") +
  annotate("text", x = 1.95e5, y = .075,
           color = cols[2], size = 5,
           label = bquote(atop("Median" ~ B[0] * ":", .(format(round(quants_combined[3]), big.mark = ",")) ~ "tons"))) +
  scale_x_continuous(label = scales::label_comma()) +
  scale_y_continuous(label = scales::percent) +
  theme_bw() 

ggsave("estimate_B0/plots/combined_histogram_sensitivity.png", combined_hist, width = 6, height = 3.5)
