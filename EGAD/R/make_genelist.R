#' Creating list of all genes in the data set.
#'
#' The function extracts the list of all genes in the data set 
#' 
#' @param gene_data_interacting 2-column matrix, each row a pair indicating a relationship or interaction
#'
#' @return list array of data labels 
#' 
#' @keywords extract gene list
#' 
#'
#' @export

make_genelist <- function(gene_data_interacting) {
    
    list <- append(as.character(gene_data_interacting[, 1]), as.character(gene_data_interacting[, 2]))
    list <- list[!duplicated(list)]
    
    return(list)
    
} 
