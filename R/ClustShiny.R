#' Graphical representation of the package
#' @import shiny
#' @import plotly
#' @import shinydashboard
#' @import dashboardthemes
#' @import clValid
#' @export
ClustShiny <- function() {
  appDir <- system.file("shiny-function", "myapp", package = "ClusteringR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ClusteringR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
