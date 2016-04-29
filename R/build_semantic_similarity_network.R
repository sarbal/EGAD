#' Builds a semantic similarity network
#'
#' The function builds a semantic similarity network given a data and labels 
#'
#' @param genes.labels matrix with rows as genes and columns as a function/label
#' @param genes array of gene IDs   
#' 
#' @return net Numeric value 
#' 
#' @keywords jaccard
#' semantic similarity 
#' network 
#'
#' @examples 
#' genes.labels <- matrix( sample(c(0,1), 100, replace=TRUE),ncol=10,nrow=10)
#' rownames(genes.labels) <- 1:10
#' genes <- 1:10
#' net <- build_semantic_similarity_network(genes.labels, genes)
#'
#' @export
#'
build_semantic_similarity_network <- function(genes.labels, genes) {
    
    # Filter on genes
    m.gl <- match(rownames(genes.labels), genes)
    f.gl <- !is.na(m.gl)
    f.gl2 <- m.gl[f.gl]
    
    # Calculate jaccard
    num <- genes.labels[f.gl, ] %*% t(genes.labels[f.gl, ])
    sum <- apply(genes.labels[f.gl, ], 1, sum)
    sum_rep <- repmat(as.matrix(sum), 1, length(sum))
    denom <- t(sum_rep) + sum_rep - num
    net <- num/denom
    t <- which(is.na(net))
    net[t] <- 0
    diag(net) <- 0
    
    return(net)
} 
