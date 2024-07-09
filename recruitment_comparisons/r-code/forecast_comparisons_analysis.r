#################################################################################

# Analyze results from forecast comparisons

# This script analyzes results from `r-code/analysis_improvements_v2/forecast_comparisons.r`

# That script outputs sr_sim.rds, this script reads that file are renders data
# visualizations (animations, heatmaps, etc.)

# ASA 2022-forecast 
# spawn years 1976-2018
# recruit years 1979-2021

# by: CL Roberts

#################################################################################

## set up

library(herringSim)
library(ggplot2)
library(dplyr)
library(data.table)
library(magrittr)
library(gganimate)
library(gifski)
library(ggrepel)
library(magick)
library(cowplot)
library(ggridges)

data("cols")

max_out <- 12
final_year <- max(caa_asa2023_forecast$Year)
sr_sim <- readRDS("recruitment_comparisons/data/sr_sim_1979-2022.rds")

# tmp <- readRDS("data/analysis_improvements_v2/sr_sim.rds")

#------------------------------------------------------------------------------------------

##### visualize results  #####

# colors used in visualizations

color_values <- c("int" = cols[1], "bh" = cols[2], "rick" = cols[3], "rick_sst" = cols[4], "rick_pdo" = "#ffae22",
                #   "rick_upw" = cols[5], "rick_soi" = cols[6], "rick_precip" = "#fd3ff4b7", 
                  "samp1" = "#5961ff",  "samp3" = "#0db004", 
                  "samptt" = "#00ffc8", # "kern" = "#9c5e18",
                  "simp" = "#ccde02", "true" = "black")

fill_values <- c("int" = cols[1], "bh" = cols[2], "rick" = cols[3], "rick_sst" = cols[4], "rick_pdo" = "#ffae22",
                #   "rick_upw" = cols[5], "rick_soi" = cols[6], "rick_precip" = "#fd3ff4b7", 
                  "samp1" = "#5961ff",  "samp3" = "#0db004", 
                  "samptt" = "#00ffc8", # "kern" = "#9c5e18",
                  "simp" = "white", "true" = "white")


#------------------------------------------------------------------------------------------

# spawner and recruit forecasts for each model

# recruits_molten <- spawners_molten <- sses <- vector(mode = "list", length = max_out)
# spawn_anim <- rec_anim <- vector(mode = "list", length = max_out)

# this loop sets up animations and saves png plots, looping over delta

# for(l in 1:max_out){
#     recruits_molten[[l]] <- sr_sim[[l]] |>
#                      select(-grep(c("spawners|upr|lwr"), names(sr_sim[[l]]))) |>
#                      as.data.table() |>
#                      melt(id.vars = c("year", "cycle", "forecast")) |>
#                         mutate(variable = recode(variable, "recruits_int" = "int", "recruits_bh" = "bh", 
#                                                  "recruits_rick" = "rick", "recruits_rick_sst" = "rick_sst",
#                                                  "recruits_rick_upw" = "rick_upw", "recruits_rick_soi" = "rick_soi",
#                                                  "recruits_rick_pdo" = "rick_pdo", "recruits_rick_precip" = "rick_precip",
#                                                  "recruits_samp1" = "samp1", "recruits_samp3" = "samp3", 
#                                                  "recruits_kern" = "kern", "recruits_simp" = "simp", 
#                                                  "recruits_true" = "true"))

#     spawners_molten[[l]] <- sr_sim[[l]] |>
#                         select(-grep("recruits|upr|lwr", names(sr_sim[[l]]))) |>
#                         as.data.table() |>
#                         melt(id.vars = c("year", "cycle", "forecast")) |>
#                         mutate(variable = recode(variable, "spawners_int" = "int", "spawners_bh" = "bh", 
#                                                  "spawners_rick" = "rick", "spawners_rick_sst" = "rick_sst",
#                                                  "spawners_rick_upw" = "rick_upw", "spawners_rick_soi" = "rick_soi",
#                                                  "spawners_rick_pdo" = "rick_pdo", "spawners_rick_precip" = "rick_precip",
#                                                  "spawners_samp1" = "samp1", "spawners_samp3" = "samp3", 
#                                                  "spawners_kern" = "kern", "spawners_simp" = "simp", 
#                                                  "spawners_true" = "true"))
#     sses[[l]] <- tapply(spawners_molten[[l]]$value - sr_sim[[l]]$spawners_true, 
#                    INDEX = spawners_molten[[l]]$variable, FUN = function(x) sum(x^2))
#     rec_anim[[l]] <- ggplot(recruits_molten[[l]], aes(x = year, y = value, group = variable)) + 
#         geom_line(aes(color = variable), size = 1.25) +
#         geom_vline(xintercept = c(final_year-l-3, final_year-l)) + 
#         theme_CL() +
#         xlim(final_year-l-3, final_year) +
#         geom_point(aes(color = variable)) +
#         scale_color_manual(values = color_values[order(sses[[l]])]) +
#         theme(legend.position = c(.2, .8)) 
#     ggsave(paste0("plots/analysis_improvements_v2/rec_forecasts/rec_forecast_", letters[l], ".png"))
#     spawn_anim[[l]] <- ggplot(spawners_molten[[l]], aes(x = year, y = value, group = variable)) + 
#         geom_line(aes(color = variable), size = 1.25) +
#         geom_vline(xintercept = c(final_year-l-3, final_year-l)) + 
#         theme_CL() +
#         xlim(final_year-l-3, final_year) +
#         geom_point(aes(color = variable)) +
#         scale_color_manual(values = color_values[order(sses[[l]])]) +
#         theme(legend.position = "bottom",   
#               legend.title = element_blank()) 
#     ggsave(paste0("plots/analysis_improvements_v2/spawn_forecasts/spawn_forecast_", letters[l], ".png"))
# }

# # this loop creates 4 animations (instead of 1 for each delta)

# for(l in seq(5, max_out, by = 5)){
#     anim_tmp <- spawn_anim[[l]] + 
#                     transition_reveal(year, range = c(final_year-l-3, final_year)) + 
#                     ease_aes("sine-in-out")
#     animate(anim_tmp, nframes = 50+(10*l), detail = 3,
#             height = 500, width = 1000, 
#             renderer = gifski_renderer())
#     anim_save(filename = paste0("animations/sr_anim_", letters[l], ".gif"))
# }

#-----------------------------------------------------------------------------------------

## visualize results - spawner and forecasts, by method type, with confidence bounds 

# this loop sets up animations and saves png plots, looping over delta

# spawn_CIs_anim <- models_gg <- samples_gg <- legend <- title <- vector(mode = "list", length = max_out)
# drop <- c("int", "rick_precip", "simp", "kern")  # drop 3 methods based on initial screening (and kern)

# for(l in 1:max_out){

#     spawners_CIs <- sr_sim[[l]] |> select(year, spawners_true,
#                                            grep("int|bh|rick|rick_sst|rick_upw|rick_soi|rick_pdo|rick_precip|samp1|samp3|samptt|kern|simp|true", 
#                                                 names(sr_sim[[l]]))) |>
#                             as.data.table() |>
#                             melt(id.vars = c("year", "spawners_true"),  
#                                  measure.vars = list(c("spawners_int", "spawners_bh", "spawners_rick",
#                                                        "spawners_rick_sst", "spawners_rick_upw", "spawners_rick_soi",
#                                                        "spawners_rick_pdo", "spawners_rick_precip", 
#                                                        "spawners_samp1", "spawners_samp3", 
#                                                        "spawners_samptt", "spawners_kern",
#                                                        "spawners_simp", "spawners_true"), 
#                                                      c("int_lwr", "bh_lwr", "rick_lwr", "rick_sst_lwr", "rick_upw_lwr",
#                                                        "rick_soi_lwr", "rick_pdo_lwr", "rick_precip_lwr", "samp1_lwr", "samp3_lwr", "samptt_lwr", "kern_lwr", "simp_lwr"), 
#                                                      c("int_upr", "bh_upr", "rick_upr", "rick_sst_upr", "rick_upw_upr",
#                                                        "rick_soi_upr", "rick_pdo_upr", "rick_precip_upr", "samp1_upr", "samp3_upr", "samptt_upr", "kern_upr", "simp_upr")),
#                                  value.name = c("estimate", "lwr", "upr"), 
#                                  variable.name = c("transition_index"), 
#                                  variable.factor = TRUE) |>
#                             mutate(model = recode(transition_index, `1` = "int", `2` = "bh", `3` = "rick",
#                                                   `4` = "rick_sst", `5` = "rick_upw", `6` = "rick_soi",
#                                                   `7` = "rick_pdo", `8` = "rick_precip", `9` = "samp1", 
#                                                   `10` = "samp3", `11` = "samptt", `12` = "kern", `13` = "simp", 
#                                                   `14` = "true"),
#                                    transition_index = replace(transition_index, 
#                                                               model == "true", NA))

#     spawners_CIs <- spawners_CIs |> filter(!model %in% drop)
#     index_factor <- factor(spawners_CIs$model,
#                             levels = c("int", "bh", "rick", "rick_sst", "rick_upw", "rick_soi", "rick_pdo",
#                                         "rick_precip", "samp1", "samp3", "samptt", "kern", "simp", "true"))
#     sses <- tapply(c(spawners_CIs$estimate - spawners_CIs$spawners_true), 
#                    INDEX = index_factor, 
#                    FUN = function(x) sum(x^2))
#     color_order <- color_values[order(sses)]
#     fill_order <- fill_values[order(sses)]

#     models_gg[[l]] <- ggplot(data = spawners_CIs, aes(x = year-3)) +
#         geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = .4) +
#         geom_line(aes(y = estimate, color = model)) + 
#         geom_vline(xintercept = c(final_year-l-3, final_year-l)) +
#         xlim(final_year-l-6, final_year-3) +
#         ylim(min(na.omit(spawners_CIs$lwr)), max(na.omit(spawners_CIs$upr))) + 
#         xlab("Spawn Year") + ylab("Spawning Biomass (metric tons)") +
#         theme_CL() + 
#         scale_color_manual(values = color_order[!names(color_order) %in% drop]) +
#         scale_fill_manual(values = fill_order[!names(color_order) %in% drop]) +
#         labs(title = "Comparison of Biomass Forecast Accuracy") +
#         theme(legend.title = element_blank(), legend.position = "bottom",
#               legend.key.size = unit(.6, "cm"),
#               legend.text = element_text(size = 14),
#               axis.text=element_text(size=14),
#               axis.title=element_text(size=18),
#               title = element_text(size = 18)) + 
#         guides(colour = guide_legend(nrow = 2)) 

#     if(l %in% 1:5) models_gg[[l]] <- models_gg[[l]] + 
#         labs(subtitle = bquote("Long training period, short forecast period" ~ delta ~ "=" ~ .(l+3)))
#     if(l %in% 6:10) models_gg[[l]] <- models_gg[[l]] + 
#         labs(subtitle = bquote("Medium training period, medium forecast period" ~ delta ~ "=" ~ .(l+3)))
#     if(l %in% 11:15) models_gg[[l]] <- models_gg[[l]] + 
#         labs(subtitle = bquote("Short training period, long forecast period" ~ delta ~ "=" ~ .(l+3)))
#     ggsave(file = paste0("plots/analysis_improvements_v2/spawn_CIs/spawn_CIs_", letters[l], ".png"), device = "png")
# }


# anim_index <- seq(5, max_out, by = 5)
# models_anim <- samples_anim <- vector(mode = "list", length = length(anim_index))

# for(l in anim_index){

#     models_anim[[l]] <- models_gg[[l]] +
#         transition_states(as.integer(transition_index)) + 
#         geom_line(aes(y = spawners_true), color = "black") + 
#         enter_fade(alpha = .05) + exit_fade(alpha = .05) +
#         shadow_mark(alpha = .05, past = TRUE, future = TRUE) 

#     models_gif <- animate(models_anim[[l]], width = 800, height = 400, fps = 50, duration = 20,
#         renderer = gifski_renderer())
#     anim_save(filename = paste0("animations/sr_CIs_anim_", letters[l], ".gif"),
#               animation = models_gif)

# }  


#-----------------------------------------------------------------------------------------

## visualize results - heatmaps of forecast errors

# data wrangling

error_list <- vector(mode = "list", length = max_out)

for(l in 1:max_out){
    error_list[[l]] <- sr_sim[[l]] |> select(!grep("lwr|upr|recruits", names(sr_sim[[l]]))) |>
                            rename(int = spawners_int, bh = spawners_bh, rick = spawners_rick, 
                                rick_sst = spawners_rick_sst, rick_pdo = spawners_rick_pdo,
                                # rick_upw = spawners_rick_upw, rick_soi = spawners_rick_soi, 
                                # rick_precip = spawners_rick_precip,
                                samp1 = spawners_samp1, samp3 = spawners_samp3, 
                                samptt = spawners_samptt, 
                                simp = spawners_simp, true = spawners_true) |>
                            as.data.table() |>
                            filter(year > final_year-l) |>
                            melt(id.vars = c("year", "true"),  
                                 measure.vars = c("int", "bh", "rick", "rick_sst", "rick_pdo",
                                                #   "rick_upw", "rick_soi", "rick_precip", 
                                                  "samp1", "samp3", "samptt",
                                                  "simp"), 
                                 value.name = "estimate", 
                                 variable.name = "model") |>
                            mutate(nyears_forecasted = year - min(year) + 1, delta = l+3,
                                    error = (estimate - true),
                                    pct_error = 100 * error / true)
}

error_df <- do.call(rbind, error_list)

cols_spectrum <- colorRampPalette(c("#ff4800", "#ffffff", "#5185ff"), space = "rgb")(3)

# locate and remove infinite rows (remove year/delta combos for all methods)

error_df <- error_df[delta < 16,] 

# summary statistics table
overall_means <- tapply(error_df$pct_error, INDEX = error_df$model, FUN = mean)
error_df$Mean <- overall_means[match(error_df$model, names(overall_means))]

error_tbl_summary <- t(data.frame(
    MPE = sapply(split(error_df, f = error_df$model), FUN = function(x) mean(x$pct_error)),
    MAPE = sapply(split(error_df, f = error_df$model), FUN = function(x) mean(abs(x$pct_error))),
    Min = sapply(split(error_df, f = error_df$model), FUN = function(x) min(x$pct_error)), 
    # Q1 = sapply(split(error_df, f = error_df$model), FUN = function(x) quantile(x$pct_error, .25)),
    # Q2 = sapply(split(error_df, f = error_df$model), FUN = function(x) median(x$pct_error)),
    # Q3 = sapply(split(error_df, f = error_df$model), FUN = function(x) quantile(x$pct_error, .75)),
    Max = sapply(split(error_df, f = error_df$model), FUN = function(x) max(x$pct_error))
))

knitr::kable(t(error_tbl_summary), format = "pipe", digits = 1, align = "c")

# ggridges plot

error_ridges <- ggplot(error_df, aes(x = pct_error, y = model)) + 
    geom_density_ridges(aes(fill = Mean), calc_ecdf = TRUE) + 
    xlim(c(-100, 200)) + 
    theme_CL() +
    xlab("Percent Error") + ylab(NULL) + labs(fill = "MPE") +
    scale_fill_gradient2(low = cols_spectrum[1], mid = cols_spectrum[2], high = cols_spectrum[3])
error_ridges <- error_ridges + labs(title = "Distribution of Percent Errors by Model")
ggsave("recruitment_comparisons/plots/error_ridges.png", device = "png")

# remove 3 forecasting methods based on initial screening

# drop <- c("int", "rick_precip", "simp")  
# error_df <- error_df |> filter(!(model %in% drop)) 
# error_df$model <- droplevels(error_df$model)

# heatmaps of forecast runs

heatmap_all <- ggplot(data = error_df, aes(x = year, y = nyears_forecasted)) +
    geom_tile(aes(fill = pct_error)) + 
    theme_CL() +
    scale_fill_gradient2(low = cols_spectrum[1], mid = cols_spectrum[2], high = cols_spectrum[3])  + 
    facet_wrap(~model) + 
    xlab("Recruit Year") + ylab("Number of Years Projected") +
    theme(axis.text.x = element_text(angle = -75, vjust = 0.5, hjust=1))
heatmap_all <- heatmap_all + labs(title = "Heat Map of Forecast Method Accuracy", 
         fill = "% Error") 
ggsave("recruitment_comparisons/plots/heatmap_all.png", device = "png")


# average errors in 3 ways

delta_df <- sapply(split(error_df, f = error_df$model), 
                        FUN = function(x) {
                            tapply(x$pct_error, INDEX = x$delta, FUN = mean)
                            }) %>% 
                    as.data.table() %>%
                    mutate(delta = unique(error_df$delta)) %>%
                    melt(id.vars = "delta", variable.name = "model", value.name = "MPE")

delta_df$MAPE <- sapply(split(error_df, f = error_df$model), 
                        FUN = function(x) {
                            tapply(abs(x$pct_error), INDEX = x$delta, FUN = mean)
                            }) %>% 
                    as.data.table() %>%
                    mutate(delta = unique(error_df$delta)) %>%
                    melt(id.vars = "delta", variable.name = "model", value.name = "MAPE") %>%
                    select(MAPE)

nyears_forecasted_df <- sapply(split(error_df, f = error_df$model), 
                                FUN = function(x) {
                                    tapply(x$pct_error, INDEX = x$nyears_forecasted, FUN = mean)
                                    }) %>% 
                    as.data.table() %>%
                    mutate(nyears_forecasted = sort(as.numeric(rownames(.)))) %>%
                    melt(id.vars = "nyears_forecasted", variable.name = "model", value.name = "MPE")
nyears_forecasted_df$MAPE <- sapply(split(error_df, f = error_df$model), 
                                FUN = function(x) {
                                    tapply(abs(x$pct_error), INDEX = x$nyears_forecasted, FUN = mean)
                                    }) %>% 
                    as.data.table() %>%
                    mutate(nyears_forecasted = sort(as.numeric(rownames(.)))) %>%
                    melt(id.vars = "nyears_forecasted", variable.name = "model", value.name = "MAPE") %>%
                    select(MAPE)


sapply(split(error_df, f = error_df$model), 
                                FUN = function(x) {
                                    tapply(abs(x$pct_error), INDEX = x$nyears_forecasted, FUN = mean)
                                    }) %>% 
                    as.data.table() %>%
                    mutate(nyears_forecasted = sort(as.numeric(rownames(.)))) %>%
                    melt(id.vars = "nyears_forecasted", variable.name = "model", value.name = "MAPE") %>%
                    select(MAPE)

nyear_tab <- data.frame(year = 1:12, sapply(split(error_df, f = error_df$model), 
                                FUN = function(x) {
                                    tapply(x$pct_error, INDEX = x$nyears_forecasted, FUN = mean)
                                    }))

years_df <- sapply(split(error_df, f = error_df$model), 
                        FUN = function(x) {
                            tapply(x$pct_error, INDEX = x$year, FUN = mean)
                            }) %>% 
                    as.data.table() %>%
                    mutate(year = (final_year-max_out+1):final_year) %>%
                    melt(id.vars = "year", variable.name = "model", value.name = "MPE")
years_df$MAPE <- sapply(split(error_df, f = error_df$model), 
                        FUN = function(x) {
                            tapply(abs(x$pct_error), INDEX = x$year, FUN = mean)
                            }) %>% 
                    as.data.table() %>%
                    mutate(year = (final_year-max_out+1):final_year) %>%
                    melt(id.vars = "year", variable.name = "model", value.name = "MAPE") %>%
                    select(MAPE)


# check if all datasets produce the same grand mean for each forecast method

# rbind(
#     # this is what they should be
#     sapply(split(error_df, f = error_df$model), FUN = function(x) mean(abs(x$pct_error))), 
#     sapply(split(delta_df, f = delta_df$model), FUN = function(x) weighted.mean(x$pct_error, w = x$delta-3)),
#     sapply(split(nyears_forecasted_df, f = nyears_forecasted_df$model), 
#                     FUN = function(x) weighted.mean(x$pct_error, w = rev(x$nyears_forecasted))),
#     sapply(split(years_df, f = years_df$model), FUN = function(x) weighted.mean(x$pct_error, w = rev(2022-x$year)))
# )


# these lines identify the best model averaged over year and projection length

delta_ls <- split(delta_df, f = delta_df$delta)
delta_bestmodel <- data.table(delta = as.integer(names(delta_ls)),
                              MAPE = sapply(delta_ls, FUN = function(x) x$model[which.min(x$MAPE)]),
                              MPE = sapply(delta_ls, FUN = function(x) x$model[which.min(abs(x$MPE))])) |>
    melt(id.vars = "delta", variable.name = "Metric", value.name = "Best")

nyears_ls <- split(nyears_forecasted_df, f = nyears_forecasted_df$nyears_forecasted)
nyears_bestmodel <- data.table(nyears = as.integer(names(nyears_ls)),
                               MAPE = sapply(nyears_ls, FUN = function(x) x$model[which.min(x$MAPE)]),
                               MPE = sapply(nyears_ls, FUN = function(x) x$model[which.min(abs(x$MPE))])) |>
    melt(id.vars = "nyears", variable.name = "Metric", value.name = "Best")

years_ls <- split(years_df, f = years_df$year)
years_bestmodel <- data.table(years = as.integer(names(years_ls)),
                              MAPE = sapply(years_ls, FUN = function(x) x$model[which.min(x$MAPE)]),
                              MPE = sapply(years_ls, FUN = function(x) x$model[which.min(abs(x$MPE))])) |>
    melt(id.vars = "years", variable.name = "Metric", value.name = "Best")


# heatmaps averaged over all forecast runs (by delta, year and projection length)

heat_delta <- ggplot(data = delta_df, aes(x = delta, y = model)) +
    geom_tile(aes(fill = MPE)) + 
    theme_CL() +
    scale_fill_gradient2(low = cols_spectrum[1], mid = cols_spectrum[2], high = cols_spectrum[3])  + 
    geom_text(aes(label = round(MPE))) +
    xlab(bquote(delta)) + ylab("Method") +
    geom_tile(data = delta_bestmodel, aes(x = delta, y = Best, color = Metric, linewidth = Metric), fill = NA) +
    scale_linewidth_discrete(range = c(2,1)) 
heat_delta <- heat_delta + labs(title = "Heat Map of Forecast Method Accuracy", 
         subtitle = bquote("Spawning Biomass Error Averaged Over" ~ delta),
         fill = "Mean Abs \n % Error")
ggsave("recruitment_comparisons/plots/heatmap_delta.png", device = "png")


heat_nyears <- ggplot(data = nyears_forecasted_df, aes(x = nyears_forecasted, y = model)) +
    geom_tile(aes(fill = MPE)) + 
    theme_CL() +
    scale_fill_gradient2(low = cols_spectrum[1], mid = cols_spectrum[2], high = cols_spectrum[3])  + 
    geom_text(aes(label = round(MPE))) +
    xlab("Number of Years Projected") + ylab("Method") +
    geom_tile(data = nyears_bestmodel, aes(x = nyears, y = Best, color = Metric, linewidth = Metric), 
              fill = NA) +
    scale_linewidth_discrete(range = c(2,1)) 
heat_nyear <- heat_nyears + labs(title = "Heat Map of Forecast Method Accuracy", 
         subtitle = "Spawning Biomass Error Averaged Over Number of Projection Years",
         fill = "Mean Abs \n % Error") 
ggsave("recruitment_comparisons/plots/heatmap_nyears.png", device = "png")


heat_years <- ggplot(data = years_df, aes(x = year-3, y = model)) +
    geom_tile(aes(fill = MPE)) + 
    theme_CL() +
    scale_fill_gradient2(low = cols_spectrum[1], mid = cols_spectrum[2], high = cols_spectrum[3])  + 
    geom_text(aes(label = round(MPE))) +
    xlab("Recruit Year") + ylab("Method") +
    geom_tile(data = years_bestmodel, aes(x = years-3, y = Best, color = Metric, linewidth = Metric), 
              fill = NA) +
    scale_linewidth_discrete(range = c(2,1)) 
heat_years <- heat_years + labs(title = "Heat Map of Forecast Method Accuracy", 
         subtitle = "Spawning Biomass Error Averaged Over Projection Year",
         fill = "Mean Abs \n % Error") 
ggsave("recruitment_comparisons/plots/heatmap_years.png", device = "png")

    


