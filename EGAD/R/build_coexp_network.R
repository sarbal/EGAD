#' Builds a coexpression network from an expressionSet
#'
#' The function generates a dense coexpression network from expression data stored as a 
#' matrix, with the genes as row labels, and samples as column labels. 
#' Correlation coefficicents are used as to weight the edges of the nodes (genes). 
#' Calls \code{\link{cor}}.  
#' 
#' @param exprs matrix of expression data  
#' @param gene.list array of gene labels 
#' @param method correlation method to use, default Spearman's rho
#' @param flag string to indicate if the network should be ranked
#'
#' @return net Matrix symmetric  
#'
#' 
#' @keywords dense network
#' coexpression
#' expressionSet
#' ExpressionSet
#'
#' @examples 
#' exprs <- matrix( rnorm(1000), ncol=10,byrow=TRUE)
#' gene.list <- paste('gene',1:100, sep='')
#' sample.list <- paste('sample',1:10, sep='')
#' rownames(exprs) <- gene.list
#' colnames(exprs) <- sample.list
#' network <- build_coexp_network(exprs, gene.list)
#'
#'
#' @export
#' 
build_coexp_network <- function(exprs, gene.list, method = "spearman", flag = "rank") {
    
    # Calculate correlation coefficients
    gene.corr = cor(t(exprs), method = method)
    n = dim(gene.corr)[1]
    
    # Create network
    if (flag == "rank") {
        net <- matrix(rank(gene.corr, na.last = "keep", ties.method = "average"), nrow = n, ncol = n)
        rownames(net) <- rownames(gene.corr)
        colnames(net) <- colnames(gene.corr)
        net <- net/max(net, na.rm = TRUE)
        diag(net) <- 1
    } else if (flag == "absrank") {
        net <- matrix(rank(abs(gene.corr), na.last = "keep", ties.method = "average"), nrow = n, ncol = n)
        rownames(net) <- rownames(gene.corr)
        colnames(net) <- colnames(gene.corr)
        net <- net/max(net, na.rm = TRUE)
        diag(net) <- 1
    } else {
        net <- gene.corr
    }
    
    return(net)
} 
