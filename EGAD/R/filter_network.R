filter_network <- function(network, min, max) {
    # Filter terms between min and max
    colsums <- apply(network, 2, sum)
    col.filter <- which((colsums > min & colsums < max))
    
    # Filter for genes with terms
    rowsums <- apply(network, 1, sum)
    row.filter <- which(rowsums > 0)
    network <- network[row.filter, col.filter]
    
    return(network)
} 
