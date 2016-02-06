#' Area under the receiver operating characteristic curve
#'
#' The function calculates the area under the receiver operating characteristic (ROC)
#' curve analytically  
#' 
#' @param scores numeric array
#' @param labels binary array
#'
#' @return auroc Numeric value 
#' 
#' @keywords ROC
#' receiver operating characteristic 
#' area
#' metric
#' analytic
#'
#' @export
#'

auroc_analytic <- function(scores, labels) {
    
    negatives <- which(labels == 0, arr.ind = T)
    scores[negatives] <- 0
    
    p <- sum(scores)
    nL <- length(labels)
    np <- sum(labels)
    nn <- nL - np
    
    auroc <- (p/np - (np + 1)/2)/nn
    
    return(auroc)
} 
