#' Plot density comparisons
#'
#' The function plots two density curves and compares their modes
#'
#' @param aucA numeric array of aurocs
#' @param aucB numeric array of aurocs
#' @param col color of lines
#' @param xlab string label
#' @param ylab string label
#' @param mode boolean to plot mode or mean
#'
#' @keywords
#' plot
#' 
#' @return null
#'
#' @examples 
#' aurocsA <- (runif(1000)+runif(1000)+runif(1000)+runif(1000))/4
#' aurocsB <- runif(1000)
#' plot_density_compare(aurocsA, aurocsB)
#'
#' @export
#'
plot_density_compare <- function(aucA, aucB, col = "lightgrey", xlab = "AUROC (neighbor voting)", 
    ylab = "Density", mode = TRUE, ...) {
    
    aucA_dens <- density(aucA, adjust = 2)
    aucA_dens$y <- aucA_dens$y/(max(aucA_dens$y))
    aucB_dens <- density(aucB, adjust = 2)
    aucB_dens$y <- aucB_dens$y/(max(aucB_dens$y))
    
    xlim <- range(aucA_dens$x, aucB_dens$x)
    ylim <- range(aucA_dens$y, aucB_dens$y)
    
    if (mode == TRUE) {
        a <- aucA_dens$x[which.max(aucA_dens$y)]
        b <- aucB_dens$x[which.max(aucB_dens$y)]
    } else {
        a <- mean(aucA)
        b <- mean(aucB)
    }
    
    box <- cbind(c(a, a, b, b), c(0, 1, 1, 0))
    
    plot(0, 0, col = 0, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, bty = "n", cex.lab = 1.5, 
        cex.axis = 1.2, ...)
    polygon(box, col = "gray84", border = 0, ...)
    lines(box[1:2, ], lty = 3, lwd = 3, col = 1)
    lines(box[3:4, ], lty = 3, lwd = 3, col = 2)
    lines(aucA_dens, lwd = 3, col = 1)
    lines(aucB_dens, lwd = 3, col = 2)
    
    return(list(a, b))
} 
