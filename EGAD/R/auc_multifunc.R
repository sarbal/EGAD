#' Calculating AUC for functional groups from ranked lists
#' 
#' The function calculates the AUC for a functional group 
#' analytically using an optimal ranked list of genes that 
#' indicates association between genes and groups.
#' 
#'  
#' @param annotations binary matrix indicating which list elements are in which functional groups.
#' @param optimallist Ranked list (multifunctionality analysis, see \code{\link{calculate_multifunc}}).
#'
#' @return aucs array of aucs for each group in annotations
#'
#' @keywords AUC
#' multifunctionality 
#' evalutation  
#'
#' @export
#'
auc_multifunc <- function(annotations, optimallist) {
    annotations <- as.matrix(annotations)
    n <- dim(annotations)[2]
    aucs <- sapply(1:n, function(i) auroc_analytic(optimallist, annotations[,i]) )
    return(aucs)
}
