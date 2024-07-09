####################################################################################

# flowchart

# author: CL Roberts

# code to generate an animation illustrating operating model forecasting procedure

####################################################################################


#### front matter ####

## attach data and packages

library(ggplot2)
library(herringSim)
library(data.table)
library(ggforce)
library(png)
library(patchwork)
library(cowplot)
library(gganimate)
library(gifski)
library(magick)

## set global variables

cols <- c("black", "#00ffff", "#0dff00", "#ff0000")

sr_sim <- readRDS("recruitment_comparisons/data/sr_sim_1979-2022.rds") 

tmp <- sr_asa2023_forecast |> as.data.table()
forecast_years <- 2007:max(tmp$year)
delta <- length(forecast_years)
tmp$forecast <- tmp$year %in% forecast_years
tmp$spawners <- sr_sim[[delta-4]]$spawners_true
tmp$recruits_mod1 <- sr_sim[[delta-4]]$recruits_simp
tmp$spawners_mod1 <- sr_sim[[delta-4]]$spawners_simp
tmp$recruits_mod2 <- sr_sim[[delta-4]]$recruits_rick_sst
tmp$spawners_mod2 <- sr_sim[[delta-4]]$spawners_rick_sst

#------------------------------------------------------------------------------

#### make basis for flowchart ####

## recruitment series

rec <- ggplot(data = tmp, aes(year, recruits)) + 
    geom_rect(aes(xmin = min(year)*.999, xmax = max(year)*1.0015, ymin = min(recruits)*.8, ymax = max(recruits)*1.035),
              fill = "#f7ede921", alpha = .05) +
    geom_line() + 
    geom_vline(xintercept = c(min(forecast_years), min(forecast_years)+3, max(forecast_years)),
               alpha = c(1, .5, 1), color = "black") + 
    annotate(geom = "text", x = 1983, y = max(tmp$recruits)/2.5, 
             label = bquote(R[t-3]), family = "serif", size = 6, fontface = "bold") +
    xlim(range(tmp$year)*c(.999, 1.0015)) +
    ylim(range(tmp$recruits)*c(.8, 1.035)) + 
    annotate(geom = "text", x = median(forecast_years), y = max(tmp$recruits)/1.5, 
                label = bquote(~delta), family = "serif") + 
    geom_segment(aes(x = min(forecast_years), xend = max(forecast_years), 
                y = max(recruits)/1.25, yend = max(recruits/1.25)),
                arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
    theme_void()
# ggsave("recruit_flowchart.png", device = "png", path = "recruitment_comparisons/flowchart_anim", width = 6, height = 2.5)

## ellipse showing biomass calculated from recruitment

ellipse <- ggplot(data = data.frame(-2:2, -2:2)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 16, b = 2.5, angle = 0), 
                 fill = "#60567749", alpha = .05, color = NA) + 
    # annotate("text", x = -10, y = 2.5, label = bquote(R[t]), family = "serif", size = 10) +
    # geom_curve(aes(x = -8, y = 2.5, xend = -4, yend = .75), curvature = -.3, angle = 30, 
    #            arrow = arrow(length = unit(0.06, "npc"))) +
    # annotate("text", x = 0, y = 0, label = bquote("Numbers-at-age" ~ - ~ "Catch-at-age"), family = "serif", size = 4) +
    # geom_curve(aes(x = 0, y = -.75, xend = 8, yend = -2.55), curvature = .5, angle = 30, 
    #            arrow = arrow(length = unit(0.06, "npc"))) +
    # annotate("text", x = 10, y = -2.5, label = bquote(B[t]), family = "serif", size = 10) +
    # geom_curve(aes(x = 7, y = 3.25, xend = -3, yend = .75), curvature = .3, angle = -60, 
    #         arrow = arrow(length = unit(0.06, "npc"))) +
    # geom_ellipse(aes(x0 = 9, y0 = 3, a = 3, b = 1, angle = 0), 
    #              fill = "white", color = "black") + 
    # annotate("text", x = 9, y = 3, label = "Survival", family = "serif", size = 2.5) +
    # geom_curve(aes(x = -5, y = -2.75, xend = 7, yend = -2.75), curvature = .3, angle = 30, 
    #         arrow = arrow(length = unit(0.06, "npc"))) +
    # geom_ellipse(aes(x0 = -8, y0 = -3, a = 4.25, b = 1.25, angle = 0), 
    #             fill = "white", color = "black") + 
    # annotate("text", x = -8, y = -2.85, label = "Maturity\nWeight-at-age", family = "serif", size = 2.5) +
    annotate("text", x = 0, y = 0, size = 4.5,
             label = bquote(B[t] == Sigma[a==3]^"8+" ~ "[(" ~ N[t*","*a] ~ "*" ~ rho[t*","*a] ~ ")" ~ "-" ~ C[t*","*a] ~ "]" ~ w[t*","*a])) +
    theme_void()
# ggsave("ellipse_flowchart.png", device = "png", path = "recruitment_comparisons/flowchart_anim", width = 3.5, height = 2)

## spawning biomass series

spawn <- ggplot(data = tmp, aes(year, spawners)) + 
    geom_rect(aes(xmin = min(year)*.999, xmax = max(year)*1.0015, ymin = min(spawners)*.8, ymax = max(spawners)*1.2),
              fill = "#f7ede921", alpha = .05) +
    geom_line() + 
    geom_vline(xintercept = c(min(forecast_years)+3, max(forecast_years)), alpha = c(.5, 1), 
               color = "black") + 
    annotate(geom = "text", x = 1983, y = max(tmp$spawners)/2, 
             label = bquote(B[t]), family = "serif", size = 6) +
    xlim(range(tmp$year)*c(.999, 1.0015)) +
    ylim(range(tmp$spawners)*c(.8, 1.2)) +
    theme_void()
# ggsave("spawner_flowchart.png", device = "png", path = "recruitment_comparisons/flowchart_anim", width = 6, height = 2.5)

## arrows representing flow of data

arrow1 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    xlim(-5, 5) + ylim(-5, 5) +
    theme_void() 
arrow2 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    xlim(-5, 5) + ylim(-5, 5) +
    theme_void() 
arrow3 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    xlim(range(tmp$year)*c(.999, 1.0015)) + ylim(-5, 5) +
    theme_void() 


#---------------------------------------------------------------------------------

#### save a series of flowcharts ####

## make folder to store pngs

flowchart_dir <- here::here("recruitment_comparisons/flowchart_anim/tmp") 

if(!dir.exists(flowchart_dir)){
    dir.create(flowchart_dir)
}


## base chart

flowchart_base <- plot_grid(
                    plot_grid(rec, 
                              NULL, 
                              spawn + 
                                annotate(geom = "text", x = median(forecast_years), y = max(tmp$spawners)/1.2, 
                                         label = bquote(~delta-3), family = "serif") + 
                                geom_segment(aes(x = min(forecast_years)+3, xend = max(forecast_years), 
                                         y = max(spawners)/1.025, yend = max(spawners/1.025)),
                                         arrow = arrow(length = unit(0.03, "npc"), ends = "both")), 
                              ncol = 1, rel_heights = c(1, .75, 1)
                              ), 
                    plot_grid(NULL, NULL, NULL, ncol = 1), 
                    ncol = 2, rel_widths = c(1, .75)
              )

ggsave(path = flowchart_dir, filename = "flowchart_base.png", 
       device = "png", width = 6, height = 4)


## model 1

anim_years <- seq(min(forecast_years)+3, max(forecast_years), by = 3)
leg <- c("Method 1" = cols[2], "Method 2" = cols[3])

arrow1 <- arrow1 + geom_curve(aes(x = -5, xend = 0, y = 0, yend = -3, color = "Method 1"),
            arrow = arrow(length = unit(0.2, "npc")), 
            angle = -90, curvature = -.5) +
    annotate(geom = "text", x = -1.5, y = 2, label = bquote(R[t] == N[t*","*3]), 
                family = "serif", size = 6, color = cols[2]) +
    scale_color_manual(values = leg) +
    theme(legend.position = c(.8, .5), 
            legend.title = element_blank())
arrow2 <- arrow2 + geom_curve(aes(x = 0, xend = -5, y = 3, yend = -2),
            arrow = arrow(length = unit(0.2, "npc")), 
            angle = -90, curvature = -.5, color = cols[2]) +
    annotate(geom = "text", x = -2.5, y = 1, 
            label = bquote(B[t]), family = "serif", size = 6, color = cols[2]) 


for(i in seq(anim_years)){
    tmp$recruits_mod1_anim <- ifelse(tmp$year < anim_years[i]+1, tmp$recruits_mod1, NA)
    tmp$spawners_mod1_anim <- ifelse(tmp$year < anim_years[i]+1, tmp$spawners_mod1, NA)

    Bt <- mean(tmp$spawners[tmp$year == anim_years[i]])
    Bt_hat <- mean(tmp$spawners_mod1_anim[tmp$year == anim_years[i]])

    rec <- rec + geom_line(aes(x = year, y = recruits_mod1_anim), data = tmp, color = cols[2]) +
        geom_line()
    rec_final <- rec + geom_point(aes(x = year[year == anim_years[i]], 
                y = recruits_mod1_anim[year == anim_years[i]]), data = tmp, 
                color = cols[2])
    spawn <- spawn + geom_line(aes(x = year, y = spawners_mod1_anim), data = tmp, color = cols[2]) +
        geom_point(aes(x = year[year == anim_years[i]], 
                y = spawners_mod1_anim[year == anim_years[i]]), data = tmp, color = cols[2]) +
        geom_line() 
    spawn_final <- spawn +
        geom_segment(aes(x = 2023.5, xend = 2023.5, y = spawners[year == anim_years[i]], 
            yend = spawners_mod1_anim[year == anim_years[i]]), 
            data = tmp, color = "black", 
            arrow = arrow(ends = "both", length = unit(0.025, "npc"))) +
        geom_vline(xintercept = anim_years[i], color = cols[4]) + 
        geom_point(aes(x = year[year == anim_years[i]], 
                y = spawners_mod1_anim[year == anim_years[i]]), data = tmp, color = cols[2]) +
        geom_point(aes(x = year[year == anim_years[i]], 
                    y = spawners[year == anim_years[i]]), color = "black") +
        annotate("text", label = bquote("Error" == .(round(100*((Bt_hat-Bt)/Bt), 1)) ~ "%"), 
                 color = "black", size = 3, angle = 270, 
                 x = 2025, 
                 y = mean(c(Bt, Bt_hat))
                )

    arrow3_final <- arrow3 +
        geom_curve(aes(x = anim_years[1]-16, xend = anim_years[i]-5, y = 0, yend = 4),
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = -90, curvature = .3) +
        geom_ellipse(aes(x0 = anim_years[1]-20, y0 = -1, a = 6, b = 2, angle = 0), fill = "white", color = "black") + 
        annotate(geom = "text", x = anim_years[1]-20, y = -1,
                label = "Environmental\nVariables", family = "serif", size = 2.5) +
        geom_curve(aes(x = anim_years[i]-3, xend = anim_years[i]-1.5, y = -5, yend = 0),
            angle = 90, curvature = -.2,
            color = ifelse(i==1, "black", cols[2])) +
        geom_curve(aes(x = anim_years[i]-1.5, xend = anim_years[i], y = 0, yend = 5),
            arrow = arrow(length = unit(0.2, "npc")), angle = 90, curvature = .2,
            color = ifelse(i==1, "black", cols[2])) +
        annotate(geom = "text", x = anim_years[i+1], y = -3, 
                label = bquote(B[t-3]), family = "serif", size = 6, color = ifelse(i==1, "black", cols[2])) 

    if(i == 1){
        flowchart_mod1_anim <- plot_grid(
                                plot_grid(rec_final, arrow3_final, spawn,
                                          ncol = 1, rel_heights = c(1, .75, 1)), 
                                plot_grid(arrow1, ellipse, arrow2, ncol = 1), 
                                ncol = 2, rel_widths = c(1, .75)
                            ) 
    } else if (i > 1){
        flowchart_mod1_anim <- plot_grid(
                                plot_grid(rec_final, arrow3_final, spawn_final, 
                                          ncol = 1, rel_heights = c(1, .75, 1)), 
                                plot_grid(arrow1, ellipse, arrow2, ncol = 1), 
                                ncol = 2, rel_widths = c(1, .75)
                            )
    } 
    ggsave(path = flowchart_dir, filename = paste0("flowchart_mod1_slide", i, ".png"), 
        device = "png", width = 6, height = 4)
}


## model 2

arrow1 <- arrow1 + geom_curve(aes(x = -5, xend = 0, y = 0, yend = -3, color = "Method 2"),
            arrow = arrow(length = unit(0.2, "npc")), 
            angle = -90, curvature = -.5) +
    annotate(geom = "text", x = -1.5, y = 2, label = bquote(R[t] == N[t*","*3]), 
                family = "serif", size = 6, color = cols[3]) +
    scale_color_manual(values = leg) +
    theme(legend.position = c(.8, .5), 
            legend.title = element_blank())
arrow2 <- arrow2 + geom_curve(aes(x = 0, xend = -5, y = 3, yend = -2),
            arrow = arrow(length = unit(0.2, "npc")), 
            angle = -90, curvature = -.5, color = cols[3]) +
    annotate(geom = "text", x = -2.5, y = 1, 
            label = bquote(B[t]), family = "serif", size = 6, color = cols[3]) 


for(i in seq(anim_years)){
    tmp$recruits_mod2_anim <- ifelse(tmp$year < anim_years[i]+1, tmp$recruits_mod2, NA)
    tmp$spawners_mod2_anim <- ifelse(tmp$year < anim_years[i]+1, tmp$spawners_mod2, NA)

    Bt <- mean(tmp$spawners[tmp$year == anim_years[i]])
    Bt_hat <- mean(tmp$spawners_mod2_anim[tmp$year == anim_years[i]])

    rec <- rec + geom_line(aes(x = year, y = recruits_mod2_anim), data = tmp, color = cols[3]) +
        geom_line() 
    rec_final <- rec + geom_point(aes(x = year[year == anim_years[i]], 
                y = recruits_mod2_anim[year == anim_years[i]]), data = tmp, 
                color = cols[3])
    spawn <- spawn + geom_line(aes(x = year, y = spawners_mod2_anim), data = tmp, color = cols[3]) +
        geom_point(aes(x = year[year == anim_years[i]], 
                y = spawners_mod2_anim[year == anim_years[i]]), data = tmp, color = cols[3]) +
        geom_line() 
    spawn_final <- spawn +
        geom_segment(aes(x = 2023.5, xend = 2023.5, y = spawners[year == anim_years[i]], 
            yend = spawners_mod2_anim[year == anim_years[i]]), 
            data = tmp, color = "black", 
            arrow = arrow(ends = "both", length = unit(0.025, "npc"))) +
        geom_vline(xintercept = anim_years[i], color = cols[4]) + 
        geom_point(aes(x = year[year == anim_years[i]], 
                y = spawners_mod2_anim[year == anim_years[i]]), data = tmp, color = cols[3]) +
        geom_point(aes(x = year[year == anim_years[i]], 
                    y = spawners[year == anim_years[i]]), color = "black") +
        annotate("text", label = bquote("Error" == .(round(100*((Bt_hat-Bt)/Bt), 1)) ~ "%"), 
                 color = "black", size = 3, angle = 270, 
                 x = 2025, 
                 y = mean(c(Bt, Bt_hat))
                )

    arrow3_final <- arrow3 +
        geom_curve(aes(x = anim_years[1]-16, xend = anim_years[i]-5, y = 0, yend = 4),
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = -90, curvature = .3) +
        geom_ellipse(aes(x0 = anim_years[1]-20, y0 = -1, a = 6, b = 2, angle = 0), fill = "white", color = "black") + 
        annotate(geom = "text", x = anim_years[1]-20, y = -1,
                label = "Environmental\nVariables", family = "serif", size = 2.5) +
        geom_curve(aes(x = anim_years[i]-3, xend = anim_years[i]-1.5, y = -5, yend = 0),
            angle = 90, curvature = -.2,
            color = ifelse(i==1, "black", cols[3])) +
        geom_curve(aes(x = anim_years[i]-1.5, xend = anim_years[i], y = 0, yend = 5),
            arrow = arrow(length = unit(0.2, "npc")), angle = 90, curvature = .2,
            color = ifelse(i==1, "black", cols[3])) +
        annotate(geom = "text", x = anim_years[i]+1, y = -3, 
                label = bquote(B[t-3]), family = "serif", size = 6, color = ifelse(i==1, "black", cols[3])) 

    if(i == 1){
        flowchart_mod2_anim <- plot_grid(
                                plot_grid(rec_final, arrow3_final, spawn,
                                          ncol = 1, rel_heights = c(1, .75, 1)), 
                                plot_grid(arrow1, ellipse, arrow2, ncol = 1), 
                                ncol = 2, rel_widths = c(1, .75)
                            ) 
    } else if (i > 1){
        flowchart_mod2_anim <- plot_grid(
                                plot_grid(rec_final, arrow3_final, spawn_final, 
                                          ncol = 1, rel_heights = c(1, .75, 1)), 
                                plot_grid(arrow1, ellipse, arrow2, ncol = 1), 
                                ncol = 2, rel_widths = c(1, .75)
                            )
    } 
    ggsave(path = flowchart_dir, filename = paste0("flowchart_mod2_slide", i, ".png"), 
        device = "png", width = 6, height = 4)
}

gifski(png_files = here::here(flowchart_dir, list.files(flowchart_dir)), 
       gif_file = "recruitment_comparisons/flowchart_anim/flowchart_anim.gif",
       delay = 1.5)

system(paste("rm -r", flowchart_dir))

# flowchart_final <- plot_grid(plot_grid(rec, arrow3_final, spawn, 
#                                           ncol = 1, rel_heights = c(1, .75, 1)), 
#                              plot_grid(arrow1, ellipse, arrow2, ncol = 1), 
#                              ncol = 2, rel_widths = c(1, .75)
#                             ) 
# ggsave("documentation/biometrics_presentation/flowchart.png", 
#     device = "png", width = 6, height = 4)
# ggsave("plots/analysis_improvements_v2/flowchart.png", 
#     device = "png", width = 6, height = 4)
