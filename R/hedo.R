#' @title   Hedonic data
#' @description This is hedonic data from a trial of 8 products within 294 customers.
#' @usage hedo
#' @format A data frame with 8 rows and 294 variables.
#' @examples
#' \dontrun{
#'  library(ClusteringR)
#'  cl=Clustering(Y=t(hedo),ClustMeth='Hierarchical',
#'  k=3,Hdismethod='euclidean',Hmethod="ward.D2",
#'  Graph=T,VarCart=F,IndCart=F,ElbowP=F )
#' }
#'
#' @references \url{https://husson.github.io/data.html}

"hedo"
