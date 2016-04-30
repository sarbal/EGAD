#' Calculate node degree
#'
#' The function calculates the node degree of a network 
#'
#' @param net numeric matrix   
#' 
#' @return node_degree numeric array 
#' 
#' @keywords node degree
#' network topology metric 
#' 
#' @examples
#' net <- cor( matrix(rnorm(1000), ncol=10)) 
#' n <- 10
#' net <- matrix(rank(net, na.last = 'keep', ties.method = 'average'), nrow = n, ncol = n)
#' net <- net/max(net, na.rm=TRUE)
#' nd <- node_degree(net)
#'  
#' @export
#'
node_degree <- function(net) {
    return(rowSums(net, na.rm = TRUE))
} 
