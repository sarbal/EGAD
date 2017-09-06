#' Downloading and filtering BIOGRID
#'
#' The function downloads the specifed version of biogrid for a particular taxon
#' 
#' @param species numeric taxon of species 
#' @param version string of biogrid version 
#' @param interactions string stating either physical or genetic interactions
#' 
#' 
#' @return biogrid data.frame with interactions  
#' 
#' @keywords biogrid download
#'
#' 
#'
#' @export
#' 
get_biogrid <- function(species = "9606", version = "3.4.131", interactions = "physical") {
    
    # Columns of entrez gene IDs
    col.filt <- c(2, 3)
    
    URL = "http://thebiogrid.org/downloads/archives/Release%20Archive/"
    filedir = paste("BIOGRID-", version, sep = "")
    filename = paste("BIOGRID-ALL-", version, ".tab2.zip", sep = "")
    fileURL = paste(URL, filedir, filename, sep = "/")
    tempfilein = paste("BIOGRID-ALL-", version, ".tab2.txt", sep = "")
    temp = tempfile()
    
    download.file(fileURL, temp, mode = "wb")
    unzip(temp, tempfilein)
    biogrid <- read.table(tempfilein, fill = TRUE, sep = "\t", quote = "")
    unlink(temp)
    
    # Convert data frame
    biogrid <- as.data.frame(biogrid)
    
    # Get taxon IDs
    ids <- unique(biogrid[, 16])
    
    # Taxon ID to select
    i <- which(ids == species)
    
    # Filter for taxon IDs i
    pairs <- biogrid[, 16] == ids[i] & biogrid[, 17] == ids[i] & !is.na(biogrid[, 16]) & !is.na(biogrid[, 
        17])
    
    # Only pick interactions (physical or genetic)
    p <- biogrid[pairs, 13] == interactions
    biogrid.all <- cbind(biogrid[pairs, ][p, ])
    
    # Filter out pairs with same IDs
    filt <- which(as.numeric(biogrid.all[, 2]) != as.numeric(biogrid.all[, 3]))
    biogrid.all <- biogrid.all[filt, ]
    
    return(biogrid.all)
} 
