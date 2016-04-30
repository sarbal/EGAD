#' Filter on columns
#'
#' The function filters out the columns of a matrix such that the size of the group
#' is exclusively between given min and max values 
#' 
#' @param network numeric matrix 
#' @param min numeric value 
#' @param max numeric value 
#' @param ids array 
#'
#' @return network numeric matrix 
#' 
#' @keywords network
#' filter
#' rows
#'
#' @examples 
#' genes.labels <- matrix( sample( c(0,1), 10000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:100, sep='')
#' genes.labels <- filter_network_cols(genes.labels,50,200)
#' 
#' genes.labels <- matrix( sample( c(0,1), 10000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:100, sep='')
#' genes.labels <- filter_network_cols(genes.labels,ids = paste('function', 1:20, sep=''))
#' 
#' 
#' @export
#'
filter_network_cols <- function(network, min = 0, max = 1, ids = NA) {
    network <- as.matrix(network)
    if (sum(is.na(ids))) {
        colsums <- colSums(network)
        col.filter <- which((colsums > min & colsums < max))
    } else {
        m = match(colnames(network), ids)
        col.filter = !is.na(m)
        
    }
    network <- network[, col.filter]
    
    return(network)
} 
