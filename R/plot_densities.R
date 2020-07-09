#' Plot densities
#'
#' The function plots multiple density curves and compares their modes
#'
#' @param hists list of histogram objects or density objects
#' @param id string 
#' @param col color for shading
#' @param xlab string x-axis label
#' @param ylab string y-axis label
#' @param mode flag indicating histogram or density 
#'
#' @keywords
#' plot density 
#'
#' @return null 
#' 
#' @examples 
#' aurocsA <- density((runif(1000)+runif(1000)+runif(1000)+runif(1000))/4)
#' aurocsB <- density((runif(1000)+runif(1000)+runif(1000))/3)
#' aurocsC <- density(runif(1000))
#' hists <- list(aurocsA, aurocsB, aurocsC)
#' temp <- plot_densities(hists,'', mode='density')
#'
#' @import graphics
#' @export
#'
plot_densities <- function(hists, id, col = c("lightgrey"), xlab = "", ylab = "Density", mode = "hist") {

    if (mode == "hist") {
        ymax <- max(sapply(1:length(hists), function(i) max(hists[[i]][, 2])))
        xrange <- range(sapply(1:length(hists), function(i) range(hists[[i]][, 1])))
        
        
        plot(0, 0, col = 0, xlim = xrange, ylim = c(0, ymax), xlab = xlab, ylab = ylab, bty = "n", 
            cex.lab = 1.5, cex.axis = 1.2, main = id)
        sapply(1:length(hists), function(i) polygon(hists[[i]], lwd = 0.5, col = make_transparent(col, 
            50)))
    } else {
        ymax <- max(sapply(1:length(hists), function(i) max(hists[[i]]$y)))
        xrange <- range(sapply(1:length(hists), function(i) range(hists[[i]]$x)))
        
        
        plot(0, 0, col = 0, xlim = xrange, ylim = c(0, ymax), xlab = xlab, ylab = ylab, bty = "n", 
            cex.lab = 1.5, cex.axis = 1.2, main = id)
        sapply(1:length(hists), function(i) lines(hists[[i]], lwd = 2, col = make_transparent(col, 
            50)))
        
    }
    
} 
