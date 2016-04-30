#' Plot distribution histogram
#'
#' The function plots a the distribution of AUROCs
#'
#' @param auc numeric aucs
#' @param b array of breaks 
#' @param col color of line
#' @param xlab string label
#' @param ylab string label
#' @param xlim range of values for xaxis 
#' @param ylim range of values for yaxis 
#' @param med boolean to plot median auc
#' @param avg boolean to plot average auc
#' @param density boolean 
#' @param bars boolena for barplot 
#'
#' @return auc list and quartiles 
#' 
#' 
#' @examples 
#' aurocs <- (runif(1000)+runif(1000)+runif(1000)+runif(1000))/4
#' d <- plot_distribution(aurocs)
#' 
#' @keywords AUROC ROC distribution
#' plot
#'
#' @export
#'
plot_distribution <- function(auc, b = 20, col = "lightgrey", xlab = "", ylab = "Density", xlim = c(0.4, 
    1), ylim = c(0, 5), med = TRUE, avg = TRUE, density = TRUE, bars = FALSE) {
    
    auc_hist <- hist(auc, plot = FALSE, breaks = b)
    bp <- boxplot(auc, plot = FALSE)

    
    if (bars == TRUE) {
      ymax <- max(auc_hist$density)
      ylim <- c(0,ymax)
      xlim <- range(auc_hist$breaks)
        plot(auc_hist, xlab = xlab,  xlim = xlim, col = col, bty = "n", cex.lab = 1.5, 
            cex.axis = 1.2, main = "")
        
    } else {
        if (density == TRUE) {
            h <- get_density(auc_hist)
            ymax <- max(auc_hist$density)
            ylim <- c(0,ymax)
            xlim <- range(auc_hist$breaks)
            
        } else {
            h <- get_counts(auc_hist)
            ymax <- max(auc_hist$counts)
            ylim <- c(0,ymax)
            xlim <- range(auc_hist$breaks)
            
        }
        plot(h, type = "l", xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, col = 0, bty = "n", 
            cex.lab = 1.5, cex.axis = 1.2)
        polygon(h, col = col, border = 0)
        lines(h, lwd = 2)
    }
    if (med == TRUE) {
        abline(v = bp$stats[c(2, 4)], lty = 3, col = "darkgrey", lwd = 3)
        abline(v = bp$stats[3], lty = 3, col = 2, lwd = 3)
    }
    if (avg == TRUE) {
        abline(v = mean(auc), lty = 2, col = 2)
    }
    
    return(list(auc_hist, bp))
} 
