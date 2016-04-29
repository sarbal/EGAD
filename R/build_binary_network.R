#' Builds a binary network
#'
#' The function creates a gene-by-gene matrix with binary entries indicating interaction (1) 
#' or no interaction (0) between the genes.
#' 
#' @param data 2-column matrix, each row a pair indicating a relationship or interaction
#' @param list string array of genes/labels/ids 
#' 
#' @return net matrix binary characterizing interactions 
#' 
#' @keywords interaction network
#' gene-by-gene 
#' 
#' @examples 
#' data <- cbind(edgeA=c('gene1','gene2'),edgeB=c('gene3','gene3'))
#' list <- c('gene1','gene2','gene3')
#' network <- build_binary_network(data,list)
#'
#' 
#' @export
#'
build_binary_network <- function(data, list) {
    
    n <- length(list)
    
    net <- matrix(0, ncol = n, nrow = n)
    
    m <- match((data[, 1]), list)
    p1 <- !is.na(m)
    m1 <- m[p1]
    
    m <- match((data[p1, 2]), list)
    p2 <- !is.na(m)
    m2 <- m[p2]
    
    net[cbind(m1[p2], m2)] <- 1
    net[cbind(m2, m1[p2])] <- 1
    
    
    net <- net + t(net)
    net[net == 2] <- 1
    diag(net) <- 1
    
    rownames(net) <- list
    colnames(net) <- list
    return(net)
} 
