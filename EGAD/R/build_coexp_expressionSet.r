#' Builds a coexpression network from an expressionSet
#'
#' The function generates a dense coexpression network from expression data stored in 
#' the expressionSet data type. Correlation coefficicents are used as to weight the edges
#' of the nodes (genes). Calls \code{\link{build_coexp_network}}.  
#' 
#' @param exprsSet data class ExpressionSet 
#' @param gene.list array of gene labels 
#' @param method correlation method to use, default Spearman's rho
#' @param flag string to indicate if the network should be ranked
#'
#' @return net Matrix symmetric  
#' 
#' @keywords dense network
#' coexpression
#' expressionSet
#' ExpressionSet
#'
#' 
#' @export
#' 
build_coexp_expressionSet <- function(exprsSet, gene.list, method = "spearman", flag = "rank") {
    
    # Check for expression data type
    if (class(exprsSet) == "ExpressionSet") {
        exprs <- exprs(exprsSet)
    } else if (class(exprsSet) == "Matrix" || class(exprsSet) == "matrix") {
        exprs <- exprsSet
    } else {
        print("Unknown exprsSet class, please make sure it is either a matrix or an expression set object")
    }
    
    # Check for gene list to filter expression data on
    if (missing(gene.list)) {
        gene.list <- rownames(exprs)
    } else {
        m <- match(rownames(exprs), gene.list)
        f.e <- !is.na(m)
        f.g <- m[f.e]
        exprs <- exprs[f.e, ]
        gene.list <- gene.list[f.g]
    }
    
    net < -build_coexp_network(exprs, method, flag)
    return(net)
} 
