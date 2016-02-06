#' Obtain expression matrix from the GEMMA database
#'
#' The function downloads and parses the expression matrix from the GEMMA 
#' database, specified by the GEO ID
#' 
#' @param gseid GEO ID of the expression experiment 
#' @param filtered flag to indicate whether or not the data is QC 
#' 
#' @return list of genes and the expression matrix 
#' 
#' @keywords expression experiment 
#' GEMMA 
#' GEO
#' GSE 
#' 
#'
#' @export
#'
get_expression_data_gemma <- function(gseid, filtered = "true") {
    # Get the text file from gemma (via url)
    
    url = paste("http://chibi.ubc.ca/Gemma/rest/experimentData/findExpressionDataByEeName/", gseid, ",", 
        filtered, sep = "")
    
    data.matrix <- read.table(url, sep = "\t", header = T, quote = "")
    
    return(list(genes, data.matrix))
} 
