#' Builds a weighted network
#'
#' The function creates a gene-by-gene matrix with binary entries indicating interaction (1) 
#' or no interaction (0) between the genes.
#' 
#' @param data 3-column matrix, each row a pair indicating a relationship or interaction, and the last column the weight
#' @param list string array of genes/labels/ids 
#' 
#' @return net matrix characterizing interactions 
#' 
#' @keywords interaction network
#' gene-by-gene 
#'
#' @examples 
#' data <- cbind(edgeA=c('gene1','gene2'),edgeB=c('gene3','gene3'), weight=c(0.5, 0.9))
#' list <- c('gene1','gene2','gene3')
#' network <- build_weighted_network(data,list)
#'
#'
#' @export
#'

build_weighted_network <- function(data, list) {
    
    n <- length(list)
    
    net <- matrix(0, ncol = n, nrow = n)
    
    m <- match((data[, 1]), list)
    p1 <- !is.na(m)
    m1 <- m[p1]
    
    m <- match((data[p1, 2]), list)
    p2 <- !is.na(m)
    m2 <- m[p2]
    
    net[cbind(m1, m2)] <- as.numeric(data[p1, ][p2, 3])
    net[cbind(m2, m1)] <- as.numeric(data[p1, ][p2, 3])
    
    
    rownames(net) <- list
    colnames(net) <- list
    return(net)
} 
