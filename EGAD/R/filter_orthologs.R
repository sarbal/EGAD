#' Filter on orthologs
#'
#' The function filters away the labels for the genes 
#' that are not in the orthologs list 
#'
#' @param annotations binary matrix
#' @param genelist array of gene ids
#' @param orthologs array to filter on
#'
#' @return annotations_filtered binary matrix
#' 
#' @keywords annotations
#' filter
#' orthologs
#' 
#' @examples 
#' genes.labels <- matrix( sample( c(0,1), 1000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:10, sep='')
#' gene.list <- paste('gene', 1:100, sep='')
#' orthologs <- paste('gene', (1:50)*2, sep='')
#' genes.labels.filt <- filter_orthologs(genes.labels, gene.list, orthologs) 
#'
#' @export
#'
filter_orthologs <- function(annotations, genelist, orthologs) {
    annotations_filtered <- annotations
    orthologs <- as.matrix(orthologs)
    m <- match(genelist, orthologs[, 1])
    f.g <- !is.na(m)
    annotations_filtered[!f.g, ] <- 0
    return(annotations_filtered)
} 
