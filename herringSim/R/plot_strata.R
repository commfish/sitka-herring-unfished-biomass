#' Plot stratification of spawner-recruit data
#'
#' This is a wrapper function for plotting the stratification of spawner-recruit
#' estimates for use in an empirical spawner-recruit relationship.
#'
#' @param recruits numeric vector of estimates of annual recruits (millions)
#' @param spawners numeric vector of estimates for spawning biomass three years
#' prior (tonnes)
#' @param strata factor vector giving the strata for the empirical spawner-recruit
#' relationship
#' @param splits numeric vector that gives the boundaries of strata. If there are k
#' unique elements in 'strata' then there should be k-1 elements in 'splits'
#' @param years numeric or character vector of years of ASA hindcast estimates,
#' for plot labeling
#' @references
#'   \insertRef{carlile1998}{herringSim}
#' @export

plot_strata <- function(recruits, spawners, strata = NULL, splits = NULL,
                        years = NULL){
  sr <- data.frame(recruits = recruits, spawners = spawners)
  out <- ggplot(sr, aes(x = spawners, y = recruits)) +
    geom_point() +
    xlab(expression(paste("Spawning Biomass (", B[y-3], ", metric tons)"))) +
    ylab("Age-3 Recruits (millions)") +
    labs(title = "Empirical Spawner-Recruit Relationship of Sitka Herring") +
    theme_CL() +
    theme(legend.position = c(.8, .8),
          legend.text = element_text(size = 8),
          legend.key.height = unit(.4, "cm"),
          legend.key.width = unit(.75, "cm"))
  if(!is.null(strata)){
    out <- out + geom_point(aes(color = strata)) +
      labs(color = "Strata")
  }
  if(!is.null(years)){
    out <- out + ggrepel::geom_text_repel(aes(label = substr(as.character(years),
                                                             start = 3, stop = 4)),
                                          max.overlaps = 30)
  }
  if(!is.null(splits)){
    out <- out + geom_vline(xintercept = splits) +
      annotate("text", x = splits-1000, y = max(recruits)-250, angle = "90", size = 3,
               label = sapply(lapply(as.list(splits),
                                     FUN = function(x) bquote(B[y-3] == .(round(x)))),
                              as.expression))
  }
  return(out)
}
