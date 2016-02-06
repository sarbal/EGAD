#' Gene identifier conversion tool
#'
#' The function maps between gene ids
#'
#' @param genelist array of gene IDs   
#' @param to string identifier, default Entrez 
#' @param from string identifier, default Ensembl
#' @param species string identifier, default 9606 (human)   
#' 
#' @return genelist_converted array of gene IDs  
#'
#' @keywords gene IDs 
#' Entrez
#' Ensembl
#' HUGO Symbol 
#'
#' @export
#' 
convert_gene_ids <- function(genelist, to = "Entrez", from = "Ensembl", species = "9606") {
    
    species_ids = c(9606, 10090)
    
    if (!sum(species_ids == species)) {
        print("Sorry, we do not have gene IDs for that species")
        return(genelist)
    } else {
        file = paste(species, ".gene_ids.Rdata", sep = "")
        load(file)
    }
    
    if (from == "Entrez") {
        m = match(genelist, attr$entrezID)
    } else if (from == "Ensembl") {
        m = match(genelist, attr$ensemblID)
    } else if (from == "Symbol") {
        m = match(genelist, attr$name)
    }
    
    f.g = !is.na(m)
    f.a = m[f.g]
    
    if (to == "Entrez") {
        genelist_converted = attr$entrezID[f.a]
    } else if (to == "Ensembl") {
        genelist_converted = attr$ensemblID[f.a]
    } else if (to == "Symbol") {
        genelist_converted = attr$name[f.a]
    }
    
    return(genelist_converted)
} 
