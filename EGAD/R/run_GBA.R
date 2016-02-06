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
#' 
#' data <- cbind(geneA=c('gene1','gene2'),geneB=c('gene3','gene3'))
#' genelist <- c('gene1','gene2','gene3')
#' network <- make_gene_network(data,genelist)
#' ontology <- cbind(genes=c('gene1','gene1','gene2','gene3','gene3','gene3'), 
#' GO=c('GO:1','GO:3','GO:2','GO:1','GO:2','GO:3'))
#' goterms <- c('GO:1','GO:2','GO:3')
#' nFold <- 3 
#' annotations <- make_annotations(ontology,genelist,goterms)
#' network <- make_gene_network(data,genelist)
#' GBA_analysis <- run_GBA(network,labels)
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
    auroc <- mean(roc.sub[, 1], na.rm = T)
    
    results <- list(roc.sub, genes, auroc)
    return(results)
    
} 
