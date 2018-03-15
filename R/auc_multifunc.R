#' Calculating AUC for functional groups from ranked lists
#' 
#' The function calculates the AUC for a functional group 
#' analytically using an optimal ranked list of genes that 
#' indicates association between genes and groups.
#' 
#'  
#' @param annotations binary matrix indicating which list elements are in which functional groups.
#'
#' @return aucs array of aucs for each group in annotations
#'
#' @keywords AUC
#' multifunctionality 
#' evalutation  
#'
#' @examples 
#' annotations <- matrix( sample(c(0,1), 100, replace=TRUE),ncol=10,nrow=10)
#' rownames(annotations) = paste('gene', 1:10, sep='')
#' colnames(annotations) = paste('label', 1:10, sep='')
#' aurocs_mf <- auc_multifunc(annotations)
#' 
#' @export
#'
auc_multifunc <- function(annotations) {
    annotations <- as.matrix(annotations)
    gene.mfs <- calculate_multifunc(annotations)
    optimallist <- as.matrix(gene.mfs[,4])
    n <- dim(annotations)[2]
    aucs <- sapply(1:n, function(i) auc_singlelist(optimallist, annotations[, i]))
    names(aucs) <- colnames(annotations)
    return(aucs)
} 
