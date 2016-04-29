#' Creating gene-by-gene network
#'
#' The function creates a gene-by-gene matrix with binary entries indicating interaction (1) 
#' or no interaction (0) between the genes.
#' 
#' @param data 2-column matrix, each row a pair indicating a relationship or interaction
#' @param list string array of genes
#' 
#' @return net matrix binary characterizing interactions 
#' 
#' @keywords interaction network
#' gene-by-gene 
#'
#' @examples 
#' gene.list <- paste('gene', 1:100, sep='')
#' data <- matrix(0,nrow=100, ncol=2)
#' data[,1] <- sample(gene.list, 100)
#' data[,2] <- sample(gene.list, 100)
#' net <- make_gene_network(data, gene.list)
#' 
#' 
#' @export
#' 
make_gene_network <- function(data, list) {
    
    n <- length(list)
    
    net <- matrix(0, ncol = n, nrow = n)
    
    m <- match((data[, 1]), list)
    p1 <- !is.na(m)
    m1 <- m[p1]
    
    m <- match((data[p1, 2]), list)
    p2 <- !is.na(m)
    m2 <- m[p2]
    
    net[cbind(m1, m2)] <- 1
    net[cbind(m2, m1)] <- 1
    diag(net) <- rep(1, length(net[, 1]))
    
    rownames(net) <- list
    colnames(net) <- list
    return(net)
} 
