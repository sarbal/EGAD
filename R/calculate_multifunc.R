#' Performing multifunctionality analysis
#'
#' The function performs multifunctionality analysis ([1]) 
#' for a set of annotated genes and creates a rank based 
#' optimallist. For annotations use an ontology that is 
#' large enough to serve as a prior (e.g. GO, Phenocarta). 
#' 
#' @param genes.labels Annotation matrix 
#'
#' @return gene.mfs Returns matrix with evaluation of gene function prediction by given labels: 
#'
#' 
#' @keywords gene ontology 
#' evaluation 
#' gene function prediction 
#'
#' @examples 
#' genes.labels <- matrix( sample(c(0,1), 100, replace=TRUE),ncol=10,nrow=10)
#' rownames(genes.labels) = paste('gene', 1:10, sep='')
#' colnames(genes.labels) = paste('label', 1:10, sep='')
#' mf <- calculate_multifunc(genes.labels)
#'
#' @export

calculate_multifunc <- function(genes.labels) {
    
    genes.labels <- as.matrix(genes.labels)
    N.in <- colSums(genes.labels, na.rm = TRUE)
    genes.labels <- as.matrix(genes.labels[, (N.in > 0)])
    N.out <- dim(genes.labels)[1] - N.in
    N.weights <- 1/(N.in * N.out)
    mf.N <- as.matrix(genes.labels %*% N.weights)
    mf.N.ranked <- rank(mf.N)
    names(mf.N.ranked) <- rownames(mf.N)
    N.sums <- rowSums(genes.labels, na.rm = TRUE)
    
    gene.mfs <- data.frame(Gene = rownames(mf.N), N.sums = N.sums, MF.score = mf.N, MF.rank = mf.N.ranked)
    return(gene.mfs)
} 
