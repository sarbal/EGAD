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
#' data <- cbind(geneA=c('gene1','gene2'),geneB=c('gene3','gene3'))
#' genelist <- c('gene1','gene2','gene3')
#' network <- make_gene_network(data,genelist)
#' ontology <- cbind(genes=c('gene1','gene1','gene2','gene3','gene3','gene3'), 
#' GO=c('GO:1','GO:3','GO:2','GO:1','GO:2','GO:3'))
#' goterms <- c('GO:1','GO:2','GO:3')
#' nFold <- 3 
#' annotations <- make_annotations(ontology,genelist,goterms)
#' network <- make_gene_network(data,genelist)
#' predicted_functions <- predictions(network,annotations)
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
    ab <- which(genes.labels != 0, arr.ind = T)
    n <- length(ab[, 1])
    
    # Get sums - mat. mul.
    sumin <- (t(network) %*% genes.labels)
    
    # Sum of all edge in network
    sumall <- matrix(apply(network, 2, sum), ncol = dim(sumin)[2], nrow = dim(sumin)[1])
    
    # Predictions
    predicts <- sumin/sumall
    return(predicts)
} 
