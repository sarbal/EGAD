#' Builds a coexpression network given a GEO ID
#'
#' The function generates a dense coexpression network from expression data stored in 
#' GEO. The expression data is downloaded from GEO. Correlation coefficicents are used 
#' as to weight the edges of the nodes (genes). Calls \code{\link{get_expression_matrix_from_GEO}}
#' and \code{\link{build_coexp_network}}. 
#' 
#' @param gseid string GEO ID of expression experiment 
#' @param gene.list array of gene labels 
#' @param method correlation method to use, default Spearman's rho
#' @param flag string to indicate if the network should be ranked
#'
#' @return net Matrix symmetric  
#'
#' 
#' @keywords dense network
#' coexpression
#' GEO
#' GSE
#'
#'
#' @export
#'
build_coexp_GEOID <- function(gseid, gene.list, method = "spearman", flag = "rank") {
    
    # Get expression data from GEO
    data <- get_expression_matrix_from_GEO(gseid)
    exprs <- data$data.matrix
    
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
