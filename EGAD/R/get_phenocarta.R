#' Downloading and filtering Phenocarta
#'
#' The function downloads the latest version of phenocarta
#' 
#' @param species string 
#' @param type string
#' 
#' @return data data.frame with phenocarta data  
#' 
#' @keywords phenocarta download
#'
#' @export
#' @import RCurl 
#'   
get_phenocarta <- function(species = "human", type = "all") {
    
    URL = "http://www.chibi.ubc.ca/Gemma/phenocarta/LatestEvidenceExport/AllPhenocartaAnnotations.tsv"
    data <- read.table(URL, fill = TRUE, sep = "\t", quote = "", header = TRUE)
    
    # Convert to data frame
    data <- as.data.frame(data)
    
    return(data)
} 
