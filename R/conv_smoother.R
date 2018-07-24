#' Plot smoothed curve
#'
#' The function plots a smoothed curve using the \code{\link{convolve}} function.  
#'
#' @param X numeric array   
#' @param Y numeric array 
#' @param window numeric value indicating size of window to use 
#' @param xlab string of x-axis label 
#' @param ylab string of y-axis label
#' @param raw boolean
#'
#' @return smoothed X,Y and std Y matrix
#' 
#' @keywords smooth
#' plot
#' 
#' @examples 
#' x <- 1:1000
#' y <- rnorm(1000)
#' conv <- conv_smoother(x,y,10)
#' 
#' @export
#' 
conv_smoother <- function(X, Y, window, raw = FALSE, output=FALSE, ...) {
    filt <- is.finite(X) & is.finite(Y) & !is.na(X) & !is.na(Y)
    X <- X[filt]
    Y <- Y[filt]
    
    n <- order(X)
    m <- X[n]
    i <- length(m)/window
    
    ymax <- max(Y)
    ymin <- min(Y)
    xmax <- max(X)
    xmin <- min(X)
    
    X_c <- m[(((1:i) * window) - window/2)]
    Y_c <- convolve(Y[n], rep(1, window), type = "filter")
    Y_c <- Y_c[(1:i) * window - (window - 1)]/window
    
    var_Y_c <- abs(convolve(Y[n]^2, rep(1, window)/window, type = "filter") - (convolve(Y[n], rep(1, 
        window)/window, type = "filter"))^2)
    std_Y_c <- var_Y_c^(1/2)
    std_Y_c <- std_Y_c[(1:i) * window - (window - 1)]
    
    plot(X_c, Y_c, ylim = c(ymin, ymax), xlim = c(xmin, xmax), pch=19, ...)
    
    polygon(c(X_c, rev(X_c)), c(Y_c - std_Y_c, rev(Y_c + std_Y_c)), border = NA, ...)
    
    if (raw == TRUE) {
        points(X, Y, pch = 19, cex = 0.1)
    }
    
    lines(X_c, Y_c, col = 1, lwd = 2)
    smoothed <- cbind(X_c, Y_c, std_Y_c)
    if(output==TRUE){ 
        return(smoothed)
    }
}
