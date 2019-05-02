#' Graphical representation of the package
#' @import shiny
#' @import plotly
#' @import factoextra
#' @export
#' @examples
#' \dontrun{
#' ClustShiny()
#' }
ClustShiny <- function() {
  appDir <- system.file("shiny-function", package = "ClusteringR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ClusteringR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}
