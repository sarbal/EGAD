#' Performing 'Guilt by Association' Analysis
#'
#' The function runs and evaluates gene function prediction based on the 
#' 'guilt by association'-principle using neighbor voting (\code{\link{neighbor_voting}})
#' [1]. As a measure of performance and significance of results, AUCs of all 
#' evaluated functional groups are calculated.
#'
#' @param network numeric array symmetric, gene-by-gene matrix 
#' @param labels numeric array
#' @param min numeric value to limit gene function size 
#' @param max numeric value to limit gene function size 
#' @param nfold numeric value, default is 3
#'
#' @return list roc.sub, genes, auroc 
#' 
#' @keywords neighbor voting 
#' guilt by association 
#' gene function prediction evaluation
#' cross validation
#' 
#' @examples
#' genes.labels <- matrix( sample( c(0,1), 1000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:10, sep='')
#' net <- cor( matrix( rnorm(10000), ncol=100), method='spearman')
#' rownames(net) <- paste('gene', 1:100, sep='')
#' colnames(net) <- paste('gene', 1:100, sep='')
#' 
#' gba <- run_GBA(net, genes.labels, min=10) 
#' 
#' @export
#'
#'
run_GBA <- function(network, labels, min = 20, max = 1000, nfold = 3) {
    
    m <- match(rownames(network), rownames(labels))
    f <- !is.na(m)
    g <- m[f]
    
    network.sub <- network[f, f]
    genes.labels <- filter_network_cols(labels[g, ], min, max)
    
    roc.sub <- neighbor_voting(genes.labels, network.sub, nfold)
    genes <- predictions(genes.labels, network.sub)
    auroc <- mean(roc.sub[, 1], na.rm = TRUE)
    
    results <- list(roc.sub, genes, auroc)
    return(results)
    
} 
