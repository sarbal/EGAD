#' Calculates the area under a curve
#'
#' The function calculates the area under the curve defined by x and y
#' 
#' @param x numeric array
#' @param y numeric array
#'
#' @return auc numeric value 
#' 
#' @keywords area under a curve
#' area
#' rolling mean 
#' 
#' @examples 
#' x <- 1:100
#' y <- 1:100 
#' auc <- get_auc(x,y)
#'  
#' @importFrom zoo rollmean
#' @export
#'
get_auc <- function(x, y) {
    o <- order(x)
    auc <- sum(diff(x[o]) * zoo::rollmean(y[o], 2))
    return(auc)
} 
