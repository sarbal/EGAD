#' Plot densities
#'
#' The function plots multiple density curves and compares their modes
#'
#' @param aucA numeric array of aucs
#' @param aucB numeric array of aucs
#'
#'
#' @keywords
#' plot
#'
#'
#' @export
#'
plot_densities <- function(hists, id, col = "lightgrey", xlab = "log2 expression FPKM", ylab = "Density",
    mode = "hist", ...) {
    
  if(mode == "hist"){
    ymax = max(sapply(1:length(hists), function(i) max(hists[[i]][,2]) ) )
    xrange = range(sapply(1:length(hists), function(i) range(hists[[i]][,1]) ) )


    plot(0, 0, col = 0, xlim = xrange, ylim = c(0,ymax), xlab=xlab, ylab=ylab, bty = "n", cex.lab = 1.5, cex.axis = 1.2, main=id)
    sapply(1:length(hists), function(i) polygon(hists[[i]], lwd=0.5, col=makeTransparent(col,50) ) )
  }
  else {
    ymax = max(sapply(1:length(hists), function(i) max(hists[[i]]$y) ) )
    xrange = range(sapply(1:length(hists), function(i) range(hists[[i]]$x) ) )


    plot(0, 0, col = 0, xlim = xrange, ylim = c(0,ymax), xlab=xlab, ylab=ylab, bty = "n", cex.lab = 1.5, cex.axis = 1.2, main=id)
    sapply(1:length(hists), function(i) lines(hists[[i]], lwd=2, col=makeTransparent(col,50) ) )

  
  }
} 
