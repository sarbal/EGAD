#' Area under the precision recall curve
#'
#' The function calculates the area under the precision-recall curve 
#' 
#' @param scores numeric array
#' @param labels binary array
#'
#' @return auprc Numeric value 
#' 
#' @keywords precision-recall  
#' area
#' metric
#'
#'
#' @export
#'
auprc <- function(scores, labels) {
    pr <- get_prc(scores, labels)
    auprc <- get_auc(pr[, 1], pr[, 2])
    return(auprc)
} 
