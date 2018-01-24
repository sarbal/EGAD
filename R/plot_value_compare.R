#' Plot value comparisons
#'
#' The function plots a scatter
#'
#' @param aucA numeric array of aucs
#' @param aucB numeric array of aucs
#' @param xlab string label
#' @param ylab string label
#' @param xlim range of values for xaxis 
#' @param ylim range of values for yaxis 
#' 
#' @return null 
#' 
#' @keywords
#' plot
#'
#' @export
#'
plot_value_compare <- function(aucA, aucB, xlab = "AUROC", ylab = "AUROC", xlim = c(0, 1), ylim = c(0, 
    1), ...) {
    
    a <- mean(aucA, na.rm = TRUE)
    b <- mean(aucB, na.rm = TRUE)
    plot(aucA, aucB, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, bty = "n", cex.lab = 1.5, 
        cex.axis = 1.2, pch = 19, ...)
    abline(0, 1, lwd = 3, col = "grey", ...)
    abline(v = a, lty = 2, col = 2, ...)
    abline(h = b, lty = 2, col = 2, ...)
} 
