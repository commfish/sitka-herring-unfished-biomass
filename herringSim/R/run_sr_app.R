#' Run SR App
#'
#' Executes the spawner-recruit app
#'
#' @export

run_sr_app <- function() {
  appDir <- system.file("shiny-examples", "sr_app", package = "herringSim")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
