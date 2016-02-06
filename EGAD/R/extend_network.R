#' Builds an extended network from a binary network
#'
#' The function extends a binary network by using the inverse
#' of the path length between nodes as a weighted edge
#'
#' @param net matrix binary and symmetric 
#' @param max numeric maximum number of jumps
#'
#' @return ext_net matrix dense and symmetric
#'
#' 
#' @keywords extended network
#' igraph
#' shortest path length
#'
#' 
#' @export

extend_network <- function(net, max = 6) {
    g <- graph.adjacency(net, mode = "undirected")
    s <- shortest.paths(g)
    s[s > max] <- NA
    s[!is.finite(s)] <- NA
    ext_net <- 1/s
    return(ext_net)
} 
