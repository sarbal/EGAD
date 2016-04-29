#' Plot ROC overlay
#'
#' The function plots a density overlay of ROCs given the scores and labels
#'
#' @param scores.mat numeric array   
#' @param labels.mat numeric array
#' @param nbins numeric value
#'
#' @return list of Z(matrix) and roc_sum (average ROC curve)
#' 
#' @keywords ROC overlay 
#' plot
#'
#' @examples 
#' genes.labels <- matrix( c(rep(1, 1000), rep(0,9000)), nrow=1000, byrow=TRUE)
#' rownames(genes.labels) = paste('gene', 1:1000, sep='')
#' colnames(genes.labels) = paste('function', 1:10, sep='')
#' 
#' scores <- matrix( rnorm(10000), nrow=1000)
#' scores <- apply(scores, 2, rank) 
#' rownames(scores) = paste('gene', 1:1000, sep='')
#' colnames(scores) = paste('function', 1:10, sep='')
#' 
#' z <- plot_roc_overlay(scores, genes.labels)
#'
#' @export
#' @import RColorBrewer
#' @import plyr
#' @import gplots
#' 
plot_roc_overlay <- function(scores.mat, labels.mat, nbins = 100) {
    n <- dim(labels.mat)[1]  # number of genes
    nn <- dim(labels.mat)[2]  # number of functions
    
    # matrix for image plot
    z <- diag(nbins + 1) * 0
    
    rocs <- lapply(1:nn, function(i) get_roc(scores.mat[, i], labels.mat[, i]))
    aurocs <- sapply(1:nn, function(i) auroc_analytic(scores.mat[, i], labels.mat[, i]))
    
    roc_sum_X <- rowMeans(sapply(1:nn, function(i) rocs[[i]][, 1]), na.rm = TRUE)
    roc_sum_Y <- rowMeans(sapply(1:nn, function(i) rocs[[i]][, 2]), na.rm = TRUE)
    roc_sum = cbind(roc_sum_X, roc_sum_Y)
    
    xy <- lapply(1:nn, function(i) unique(round(rocs[[i]], 3)) * nbins)
    xy2 <- cbind(unlist(lapply(1:nn, function(i) xy[[i]][, 1])), unlist(lapply(1:nn, function(i) xy[[i]][, 
        2])))
    rownames(xy2) <- NULL
    xy2 <- data.frame(xy2)
    xy3 <- count(xy2)
    z[cbind(xy3[, 1], xy3[, 2]) + 1] <- xy3[, 3]
    
    Z <- log10(z)
    Z[!is.finite(Z)] <- 0
    image(Z, col = colorpanel(nbins, "white", "black"), xlab = "FPR", ylab = "TPR", bty = "n", axes = FALSE)
    axis(1)
    axis(2)
    
    lines(roc_sum, lty = 2, col = 1)
    return(list(Z, roc_sum, aurocs, rocs))
} 
