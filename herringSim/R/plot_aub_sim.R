#' Plot AUB Simulation
#'
#' This is a wrapper function for plotting the results of average unfished
#' biomass simulations. AUB (and 25 percent harvest threshold) is computed as the mean
#' biomass of all years after the cumulative mean series converges.
#'
#' @param B a data frame returned by the function 'B_sim()'
#' @export


plot_aub_sim <- function(B){

  molten_data <- reshape2::melt(B, id.vars = c("YEAR"))
  year_converges <- stats::na.omit(B)[1,"YEAR"]
  threshold <- unique(stats::na.omit(B)[1,"AUB.25"])
  aub <- 4*threshold
  data("cols", envir = environment())

  num_years <- max(molten_data[,"YEAR"])
  legend_text <- c("Simulated Biomass",
                   paste0("Cumulative Average, years 1-", num_years),
                   paste0("AUB, years ", year_converges, "-", num_years),
                   paste0("25% AUB, years ", year_converges, "-", num_years))
  molten_data[,"variable"] <- factor(molten_data[,"variable"],
                                     levels = c("B", "B_cumavg", "AUB", "AUB.25"))

  sim <- ggplot(data = molten_data, aes(x = YEAR)) +
    geom_line(aes(y = value, color = variable, linetype = variable), na.rm = TRUE) +
    scale_linetype_manual(name = "variable",
                          values = 1:4,
                          labels = legend_text) +
    xlab("Simulation Year") +
    ylab("Spawning Biomass (tons)") +
    labs(title = "Simulated Average Unfished Biomass of Sitka Herring") +
    annotate("text", x = .25*num_years,
             y = .9*max(stats::na.omit(molten_data)[,"value"]),
             color = cols[2], size = 5,
             label = paste("Threshold: ",
                           format(round(threshold), big.mark = ","),
                           "tons", "\n",
                           "AUB: ",
                           format(round(aub), big.mark = ","), "tons")) +
    theme_CL() +
    scale_color_manual(name = "variable",
                       values = c("B" = "gray75", "B_cumavg" = "black",
                                  "AUB" = "black", "AUB.25" = "black"),
                       labels = legend_text) +
    theme(legend.title = element_blank(),
          legend.position = c(.75, .85),
          legend.text = element_text(size = 8),
          legend.key.height = unit(.4, "cm"),
          legend.key.width = unit(.75, "cm"))

  return(sim)
}
