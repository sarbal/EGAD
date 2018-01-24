#' Plot precision recall curve
#'
#' The function calculates the precision and recall and plots the curve
#' 
#' @param scores numeric array
#' @param labels binary array
#'
#' @return prc numeric arrays
#'
#'
#' @keywords PRC
#' precision
#' recall
#' metric
#' plot
#'
#' @examples
#' labels <- c(rep(0,10))
#' labels[c(1,3,5)] <- 1 
#' scores <- 10:1
#' roc <- plot_prc(scores, labels)
#'
#'
#' @export
#' 
plot_prc <- function(scores, labels, ...) {
    prc <- get_prc(scores, labels)
    recall <- prc[,1]
    precision <- prc[, 2]
    prc.null <- sum(labels)/length(labels)
    plot(recall, precision, type = "l", xlab = "Recall", ylab = "Precision", bty = "n", ...)
    abline( h = prc.null, col = 2, lty=2, ...)
    return(prc)
} 
