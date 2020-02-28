#' Area under the receiver operating characteristic curve
#'
#' The function calculates the area under the receiver operating characteristic (ROC)
#' curve analytically  
#' 
#' @param ranks numeric array
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
#' @examples 
#' labels <- c(rep(0,10))
#' labels[c(1,3,5)] <- 1 
#' scores <- 10:1
#' auroc <- auroc_analytic(scores, labels)
#'
#' @export
#'

auroc_analytic <- function(ranks, labels) {
    
    negatives <- which(labels == 0, arr.ind = TRUE)
    ranks[negatives] <- 0
    
    p <- sum(ranks, na.rm = TRUE)
    nL <- length(labels)
    np <- sum(labels, na.rm = TRUE)
    nn <- nL - np
    
    auroc <- (p/np - (np + 1)/2)/nn
    
    return(auroc)
} 
