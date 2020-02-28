#' Build receiver operating characteristic curve
#'
#' The function calculates the FPR and TRPR for the receiver 
#' operating characteristic (ROC)
#' 
#' @param ranks numeric array
#' @param labels binary array
#'
#' @return FPR,TPR numeric arrays  
#'
#' 
#' @keywords ROC
#' receiver operating characteristic 
#' metric
#' FPR
#' TPR
#' 
#' @examples
#' labels <- c(rep(0,10))
#' labels[c(1,3,5)] <- 1 
#' scores <- 10:1
#' ranks <- rank(scores)
#' roc <- get_roc(ranks, labels)
#'
#' 
#'
#' @export
#' 
get_roc <- function(ranks, labels) {
    o <- order(ranks, decreasing = TRUE)
    h1 <- labels[o]
    h2 <- !labels[o]
    
    tpr <- cumsum(h1)/sum(h1)
    fpr <- cumsum(h2)/sum(h2)
    
    return(cbind(fpr, tpr))
} 
