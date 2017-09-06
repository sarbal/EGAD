#' Creating gene annotations
#'
#' The function annotates a list of genes according to a given ontology. 
#' It creates a binary matrix associating genes (rows) with labels (columns).
#' 
#' @param data 2-column matrix, each row a pair indicating a relationship or interaction
#' @param listA string array of genes 
#' @param listB string array of labels/functions 
#' 
#' @return net matrix binary 
#' 
#' @keywords gene labels
#' annotations 
#' gene to function  
#' gene ontology
#'  
#' @examples 
#' gene.list <- paste('gene', 1:100, sep='')
#' labels.list <- paste('labels', 1:10, sep='')
#' data <- matrix(0,nrow=100, ncol=2)
#' data[,1] <- sample(gene.list, 100, replace=TRUE)
#' data[,2] <- sample(labels.list, 100, replace=TRUE)
#' net <- make_annotations(data, gene.list, labels.list)
#'   
#'  
#' @export
#' 
make_annotations <- function(data, listA, listB) {
    nr <- length(listA)
    nc <- length(listB)
    
    net <- matrix(0, ncol = nc, nrow = nr)
    
    m <- match((data[, 1]), listA)
    p1 <- !is.na(m)
    m1 <- m[p1]
    
    m <- match((data[p1, 2]), listB)
    p2 <- !is.na(m)
    m2 <- m[p2]
    
    net[cbind(m1[p2], m2)] <- 1
    
    rownames(net) <- listA
    colnames(net) <- listB
    
    return(net)
} 
