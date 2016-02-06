#' Build receiver operating characteristic curve
#'
#' The function calculates the FPR and TRPR for the receiver 
#' operating characteristic (ROC)
#' 
#' @param scores numeric array
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
#'
#' @export
#' 
get_roc <- function(scores, labels) {
    o <- order(scores, decreasing = T)
    h1 <- labels[o]
    h2 <- !labels[o]
    
    tpr <- cumsum(h1)/sum(h1)
    fpr <- cumsum(h2)/sum(h2)
    # pval = wilcox.test(scores[labels], scores[!labels]) print(get_auc(fpr,tpr) )
    return(cbind(fpr, tpr))
} 
