#' Performing multifunctionality analysis
#'
#' The function performs multifunctionality analysis ([1]) 
#' for a set of annotated genes and creates a rank based 
#' optimallist. For annotations use an ontology that is 
#' large enough to serve as a prior (e.g. GO, Phenocarta). 
#' 
#' @param genes.labels Annotation matrix 
#'
#' @return Returns matrix with evaluation of gene function prediction by given labels: 
#'
#' @author 
#' 
#' @examples 
#' data <- cbind(genes=c('gene1','gene1','gene2','gene3','gene3','gene3'), 
#' GO=c('GO:1','GO:3','GO:2','GO:1','GO:2','GO:3'))
#' listA <- c('gene1','gene2','gene3')
#' listB <- c('GO:1','GO:2','GO:3')
#' annotations <- make_annotations(data,listA,listB)
#' multifunc_assessment <- calculate_multifunc(annotations)
#' 
#' @keywords gene ontology 
#' evaluation 
#' gene function prediction 
#'
#' @export

calculate_multifunc <- function(genes.labels) {
    
    genes.labels <- as.matrix(genes.labels)
    N.in <- colSums(genes.labels, na.rm = T)
    
    genes.labels <- as.matrix(genes.labels[, (N.in > 0)])
    N.in <- colSums(genes.labels, na.rm = T)
    N.out <- dim(genes.labels)[1] - N.in
    N.weights <- 1/(N.in * N.out)
    mf.N <- as.matrix(genes.labels %*% N.weights)
    mf.N.ranked <- rank(mf.N)
    names(mf.N.ranked) <- rownames(mf.N)
    N.sums <- rowSums(genes.labels, na.rm = T)
    
    return(cbind(rownames(mf.N), N.sums, mf.N, mf.N.ranked))
} 
