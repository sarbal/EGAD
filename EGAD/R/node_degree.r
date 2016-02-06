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
#' @export
#'
node_degree <- function(net) {
    rowSums(net, na.rm = T)
} 
