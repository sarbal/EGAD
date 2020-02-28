#' Plot receiver operating characteristic curve
#'
#' The function calculates the FPR and TRPR for the receiver
#' operating characteristic (ROC) and plots the curve
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
#' @examples
#' labels <- c(rep(0,10))
#' labels[c(1,3,5)] <- 1 
#' scores <- 10:1
#' roc <- plot_roc(scores, labels)
#'
#' @import graphics
#' @export
#' 
plot_roc <- function(scores, labels) {
    roc <- get_roc(scores, labels)
    fpr <- roc[, 1]
    tpr <- roc[, 2]
    plot(fpr, tpr, type = "l", xlab = "FPR", ylab = "TPR", bty = "n")
    return(roc)
} 
