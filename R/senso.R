#' @title Sensory data
#' @usage senso
#' @description  This is sensory data from a professional trial of products
#'
#' @format A data frame with 192 rows and 294 variables
#'
#' @examples
#' \dontrun{
#' library(ClusteringR)
#' EPM(Y=t(hedo),X=senso,ModelType='Quadratic',
#' nbpoints=50,Graphpred=FALSE,Graph2D=FALSE,
#' Graph3D=FALSE,statistic.Value.Type='rsquared')}
#'
#' @references \url{https://husson.github.io/data.html}
#'
'senso'
