#' Filter on columns
#'
#' The function filters out the columns of a matrix such that the size of the group
#' is exclusively between given min and max values 
#' 
#' @param network numeric matrix 
#' @param min numeric value 
#' @param max numeric value 
#'
#' @return network numeric matrix 
#' 
#' @keywords network
#' filter
#' rows 
#'
#' @export
#'
filter_network_cols <- function(network, min, max) {
    network <- as.matrix(network)
    colsums <- colSums(network)
    col.filter <- which((colsums > min & colsums < max))
    
    network <- network[, col.filter]
    
    return(network)
} 
