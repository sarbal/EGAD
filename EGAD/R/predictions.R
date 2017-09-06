#' Performing Gene Function Prediction
#'
#' The function performs gene function prediction on the whole data set
#' using the 'guilt by association'-principle ([1]).
#'
#' @param genes.labels numeric array
#' @param network numeric array symmetric, gene-by-gene matrix 
#'
#' @return scores numeric matrix  
#' 
#' @keywords neighbor voting 
#' guilt by association 
#' gene function prediction
#'
#' @examples 
#' genes.labels <- matrix( sample( c(0,1), 1000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:10, sep='')
#' net <- cor( matrix( rnorm(10000), ncol=100), method='spearman')
#' rownames(net) <- paste('gene', 1:100, sep='')
#' colnames(net) <- paste('gene', 1:100, sep='')
#' 
#' preds <- predictions(genes.labels, net) 
#' 
#' @export
#'
#'
predictions <- function(genes.labels, network) {
    genes.labels <- as.matrix(genes.labels)
    
    # Filter for common genes between network and labels
    ord <- order(rownames(network))
    network <- network[ord, ord]
    
    ord <- order(rownames(genes.labels))
    genes.labels <- as.matrix(genes.labels[ord, ])
    
    match.lab <- match(rownames(genes.labels), rownames(network))
    filt.lab <- !is.na(match.lab)
    filt.net <- match.lab[filt.lab]
    network <- network[filt.net, filt.net]
    genes.labels <- as.matrix(genes.labels[filt.lab, ])
    
    # genes.label : needs to be in 1s and 0s
    l <- dim(genes.labels)[2]
    g <- dim(genes.labels)[1]
    ab <- which(genes.labels != 0, arr.ind = TRUE)
    n <- length(ab[, 1])
    
    # Get sums - mat. mul.
    sumin <- (t(network) %*% genes.labels)
    
    # Sum of all edge in network
    sumall <- matrix(apply(network, 2, sum), ncol = dim(sumin)[2], nrow = dim(sumin)[1])
    
    # Predictions
    predicts <- sumin/sumall
    return(predicts)
} 
