####################################################################################

# flowchart

# author: CL Roberts

# code to generate flowchart illustrating operating model forecasting procedure

####################################################################################

library(ggplot2)
library(herringSim)
library(data.table)
library(ggforce)
library(png)
library(patchwork)
library(cowplot)
library(dplyr)

sr_sim <- readRDS("recruitment_comparisons/data/sr_sim_1979-2022.rds") 

tmp <- data.frame(year = 1982:2022, recruits = NA, spawners = NA)
tmp$recruits <- naa_asa2023_forecast |>
    filter(between(Year, 1979, 2019)) |>
    select(age3) |>
    unlist()
tmp$spawners <- sr_asa2023_forecast |>
    filter(between(year, 1982, 2022)) |>
    select(spawners) 
final_year <- max(tmp$year)
forecast_years <- 2004:final_year
delta <- length(forecast_years)
maxN <- max(order(tmp$year))
tmp$forecast <- tmp$year %in% forecast_years[-1:-3]
tmp$spawners <- sr_sim[[delta-4]]$spawners_true[-1:-3]
tmp$recruits_mod1 <- sr_sim[[delta-4]]$recruits_simp[-(maxN-2):-maxN]
tmp$spawners_mod1 <- sr_sim[[delta-4]]$spawners_simp[-1:-3]
tmp$recruits_mod2 <- sr_sim[[delta-4]]$recruits_rick_sst[-(maxN-2):-maxN]
tmp$spawners_mod2 <- sr_sim[[delta-4]]$spawners_rick_sst[-1:-3]

rec <- ggplot(data = tmp, aes(year, recruits)) + 
    geom_rect(aes(xmin = min(year)*.999, xmax = max(year)*1.0015, ymin = min(recruits)*.8, ymax = max(recruits)*1.035),
              fill = "grey95", alpha = .05) +
    geom_line(aes(x = year, y = recruits_mod1), linetype = "dashed") +
    geom_line(aes(x = year, y = recruits_mod2), linetype = "dotted") +
    geom_line() + 
    geom_vline(xintercept = c(min(forecast_years)+3, max(forecast_years)),
               color = c("black", "black")) + 
    geom_point(aes(x = year[year == max(year)], 
                   y = recruits[year == max(year)]), color = "black") +
    geom_segment(aes(x = min(forecast_years)+3, xend = max(forecast_years), 
                     y = max(recruits)/2, yend = max(recruits/2)),
                     arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
    annotate(geom = "text", x = median(forecast_years)+2, y = max(tmp$recruits)/1.5, 
             label = bquote(~italic("\u03B4")), family = "serif") +
    annotate(geom = "text", x = 1985, y = max(tmp$recruits)/2.5, 
             label = bquote(italic(R[y])), family = "serif", size = 6) +
    xlim(range(tmp$year)*c(.999, 1.0015)) +
    ylim(range(tmp$recruits)*c(.8, 1.035)) +
    theme_void()
# ggsave("recruit_flowchart.png", device = "png", path = "plots/analysis_improvements_v2", width = 6, height = 2.5)

ellipse <- ggplot(data = data.frame(-2:2, -2:2)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 14, b = 4, angle = 0), 
                 fill = "grey95", alpha = .05, color = NA) + 
    # annotate("text", x = -7, y = 2.5, label = bquote(R[y]), family = "serif", size = 10) +
    # geom_curve(aes(x = -5, y = 2.5, xend = 0, yend = .75), curvature = -.5, angle = 75, 
    #            arrow = arrow(length = unit(0.06, "npc"))) +
    # annotate("text", x = 0, y = 0, label = bquote("Numbers-at-age" ~ - ~ "Catch"), family = "serif", size = 7) +
    # geom_curve(aes(x = 0, y = -.75, xend = 5, yend = -2.5), curvature = .5, angle = 30, 
    #            arrow = arrow(length = unit(0.06, "npc"))) +
    # annotate("text", x = 7, y = -2.5, label = bquote(B[y]), family = "serif", size = 10) +
    annotate("text", x = 0, y = 0, size = 4.5,
             label = bquote(italic(B[y]) == Sigma[italic(a)==3]^"8+" ~ "[(" ~ italic(N[y*","*a]) ~ "*" ~ italic("\u03C1"[a]) ~ ")" ~ "-" ~ italic(C[y*","*a]) ~ "]" ~ italic(w[y*","*a])), family = "serif") +
    theme_void()
# ggsave("ellipse_flowchart.png", device = "png", path = "plots/analysis_improvements_v2", width = 3.5, height = 2)


spawn <- ggplot(data = tmp, aes(year, spawners)) + 
    geom_rect(aes(xmin = min(year)*.999, xmax = max(year)*1.0015, ymin = min(spawners)*.8, ymax = max(spawners)*1.2),
              fill = "grey95", alpha = .05) +
    geom_line(aes(x = year, y = spawners_mod1), linetype = "dashed") +
    geom_line(aes(x = year, y = spawners_mod2), linetype = "dotted") +
    geom_line() + 
    geom_vline(xintercept = c(min(forecast_years)+3, max(forecast_years)),
               alpha = c(1, 1),
               color = c("black", "black")) + 
    geom_point(aes(x = year[year == max(year)], 
                   y = spawners[year == max(year)]), color = "black") +
    # geom_segment(aes(x = min(forecast_years)+3, xend = max(forecast_years), 
    #                  y = max(spawners)/4, yend = max(spawners/4)),
    #                  arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
    # annotate(geom = "text", x = median(forecast_years)+1, y = max(tmp$spawners)/6, 
    #          label = bquote(~delta - 3), family = "serif") +
    annotate(geom = "text", x = 1985, y = max(tmp$spawners)/2.25, 
             label = bquote(italic(B[y])), family = "serif", size = 6) +
    xlim(range(tmp$year)*c(.999, 1.0015)) +
    ylim(range(tmp$spawners)*c(.8, 1.2)) +
    theme_void()
# ggsave("spawner_flowchart.png", device = "png", path = "plots/analysis_improvements_v2", width = 6, height = 2.5)

arrow1 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    geom_curve(aes(x = -5, xend = 0, y = 0, yend = -3), linetype = "dashed",
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = -90, curvature = -.5) +
    annotate(geom = "text", x = -.5, y = 3.5, 
             label = bquote(italic(R[y]) == italic(N[y*","*3]) ~ "where"), family = "serif", size = 4) +
    annotate(geom = "text", x = 2.5, y = 1.5, 
             label = bquote(italic(N[y*","*a]) == italic(S[y-1]) * italic(N[y-1*","*a-1])), family = "serif", size = 4) +
    geom_curve(aes(x = -5, xend = -.5, y = 0, yend = -3), linetype = "dotted",
               arrow = arrow(length = unit(0.2, "npc")), 
                angle = 90, curvature = -.2) + 
    xlim(-5, 5) + ylim(-5, 5) +
    theme_void() 
arrow2 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    geom_curve(aes(x = 0, xend = -5, y = 3, yend = -2, linetype = "Method 1"),
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = -90, curvature = -.5) +
    annotate(geom = "text", x = 0, y = -4, 
             label = bquote(italic(B[y])), family = "serif", size = 4) +
    geom_curve(aes(x = 0, xend = -5, y = 3, yend = -1, linetype = "Method 2"),
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = 90, curvature = -.2) + 
    scale_linetype_manual(values = c("Method 1" = "dashed", "Method 2" = "dotted")) +
    xlim(-5, 5) + ylim(-5, 5) +
    theme_void() +
    theme(legend.position = c(.8, .5), 
        legend.title = element_blank())  
arrow3 <- ggplot(data = data.frame(-10:10, -10:10)) + 
    geom_curve(aes(x = final_year-7, xend = final_year-5.5, y = -5, yend = 0),
            angle = 90, curvature = -.2, linetype = "dashed") +
    geom_curve(aes(x = final_year-5.5, xend = final_year-4, y = 0, yend = 5),
        arrow = arrow(length = unit(0.2, "npc")), angle = 90, curvature = .2,
        linetype = "dashed") +
    geom_curve(aes(x = final_year-10, xend = final_year-6, y = -5, yend = 5),
        arrow = arrow(length = unit(0.2, "npc")), angle = -70, curvature = -.2,
        linetype = "dotted") +
    annotate(geom = "text", x = final_year-3, y = -1, 
            label = bquote(italic(B[y-3])), fontface = "italic", family = "serif", size = 4) +
    geom_curve(aes(x = final_year-30, xend = final_year-13, y = 0, yend = 4),
               arrow = arrow(length = unit(0.2, "npc")), 
               angle = 150, curvature = .4) +
    geom_ellipse(aes(x0 = final_year-32, y0 = -1, a = 8, b = 3, angle = 0), fill = "white", color = "black") + 
    annotate(geom = "text", x = final_year-32, y = -1,
            label = "Environmental\nVariables", family = "serif", size = 3.5) +
    xlim(range(tmp$year)*c(.999, 1.0015)) + ylim(-5, 5) +
    theme_void()

flowchart <- plot_grid(plot_grid(rec, arrow3, spawn, ncol = 1, rel_heights = c(1, .75, 1)), 
              plot_grid(arrow1, ellipse, arrow2, ncol = 1), ncol = 2, rel_widths = c(1, .75))
ggsave("recruitment_comparisons/plots/flowchart.png", device = "png", width = 6, height = 3)
