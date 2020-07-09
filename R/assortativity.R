#' Calculating network assortativity
#'
#' The function calculates the assortativity of a network, 
#' that measures the preference of interactions between similar nodes. 
#' As in most literature, 'similarity' is here defined in terms of node degrees. 
#' 
#' @param network matrix indicating network structure (symmetric) 
#'
#' @return Numeric value 
#'
#' @keywords assortativity 
#' network properties 
#' topology 
#'
#' @examples
#' network <- matrix( sample(c(0,1),36, replace=TRUE), nrow=6,byrow=TRUE)
#' assort_value <- assortativity(network)
#' 
#' @import stats
#' @export
#'

## Adapted from assort.m
assortativity <- function(network) {
    
    # if( missing( network) ) { stop('Need to enter network.') }
    diag(network) <- 0
    node_degree <- rowSums(network)
    
    
    # Binary network if( unique(network) == 2) { ### too slow in R, need another condition
    if (sum(network) == sum(network == 1)) {
        indices <- which(network == 1, arr.ind = TRUE)
        assort <- cor(node_degree[indices[, 1]], node_degree[indices[, 2]], method = "s")
    } else {
        # Weighted network
        indices <- which(!is.na(network), arr.ind = TRUE)
        w <- network[indices]/sum(network[indices])
        x <- node_degree[indices[, 1]] - sum(node_degree[indices[, 1]] * w)
        y <- node_degree[indices[, 2]] - sum(node_degree[indices[, 2]] * w)
        vx <- sum(w * x * x)
        vy <- sum(w * y * y)
        vxy <- sum(y * x * w)
        assort <- vxy/sqrt(vx * vy)
    }
    return(assort)
} 
