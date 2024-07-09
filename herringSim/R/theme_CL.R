#' CL's default theme
#'
#' This function globally sets plot parameters
#'
#' @export


theme_CL <- function(){

  cols <<- RColorBrewer::brewer.pal(8, "Set2")

  out <- list(theme_bw(), scale_fill_brewer(palette="Set2"),
              scale_color_brewer(palette="Set2"),
              theme(axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14)))

  return(out)

}
