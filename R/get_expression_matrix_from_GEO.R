#' Obtain expression matrix from GEO database
#'
#' The function downloads and parses the expression matrix from the GEO file 
#' specified by the GEO ID
#' 
#' @param gseid GEO ID of the expression experiment 
#' 
#' @return list of genes and the expression matrix 
#' 
#' @keywords expression experiment 
#' GEO
#' GSE 
#' 
#' @import GEOquery limma impute MASS stats graphics grDevices
#' 
#' @export
#' 
#'
get_expression_matrix_from_GEO <- function(gseid) {
    
    # Get the family_soft.gz file for the GSEid
    gseSOFT <- getGEO(GEO = gseid, GSEMatrix = FALSE)
    
    # Get the properties of the microarray samples
    descrip = Columns(GSMList(gseSOFT)[[1]])$Description[2]
    names = names(GSMList(gseSOFT))
    platforms = lapply(GSMList(gseSOFT), function(x) {
        Meta(x)$platform
    })
    gplid = unlist(unique(platforms))[1]  # take the first platform, sometimes mutliple platforms, this will fail s
    
    # Get the probeset and corresponding ORFs This is not consistent across platforms (ie the gplids),
    # may cause issues
    probesets <- Table(GPLList(gseSOFT)[[gplid]])$ID
    ORF <- Table(GPLList(gseSOFT)[[gplid]])$ENTREZ_GENE_ID
    # ORF <- Table(GPLList(gseSOFT)[[gplid]])$Entrez_Gene_ID
    
    # For each sample, get the expression levels for the probes
    data.matrix <- do.call("cbind", 
                           lapply(GSMList(gseSOFT), 
                                           function(x) { 
                                               tab <- Table(x)
                                               mymatch <- match(probesets, tab$ID_REF)
                                               return(tab$VALUE[mymatch])
                                            }
                            ))
    
    # Sometimes the data is not a numeric variable, convert
    data.matrix <- apply(data.matrix, 2, function(x) {
        as.numeric(as.character(x))
    })
    
    # Label rows and columns
    rownames(data.matrix) <- probesets
    colnames(data.matrix) <- names(GSMList(gseSOFT))
    
    # Use probes with gene ids, filter out probes with multiple ORFs and probes with no ORFs
    use_probe <- which(is.na(ORF) == FALSE & match(ORF, "", nomatch = 0) == 0 & regexpr("///", ORF) == 
        -1)
    data.matrix <- data.matrix[use_probe, ]
    rownames(data.matrix) <- ORF[use_probe]
    
    # Check to see if data is log2 transformed
    Med <- median(data.matrix, na.rm = TRUE)
    if (Med > 16) { data.matrix <- log2(data.matrix)}
    
    
    # Normalize between arrays so that the intensities or log-ratios have similar distributions
    na.length <- length(which(is.na(data.matrix) == TRUE))
    if (na.length > 0) {
        data.matrix <- impute.knn(data.matrix)$data
    }
    data.matrix <- normalizeBetweenArrays(data.matrix)
    
    # Aggregate multiple genes, using median expression value
    tmp <- aggregate(data.matrix, list(rownames(data.matrix)), median)
    data.matrix <- as.matrix(tmp[, -1])
    genes <- tmp[, 1]
    rownames(data.matrix) <- genes
    
    # Clean up
    rm(tmp, na.length, use_probe, ORF)
    
    return(list(genes, data.matrix))
} 
