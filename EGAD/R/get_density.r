#' Get density
#'
#' The function formats the density distribution from the histogram function
#'
#' @param hist histogram
#'
#' @return array
#'
#' @keywords histogram
#' plot
#'
#' @export
#'
get_density <- function(hist) {
    x = sort(rep(hist$breaks, 2))
    y = matrix(rbind(hist$density, hist$density))
    y = c(0, y, 0)
    
    return(cbind(x, y))
} 
