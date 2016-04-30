#' Filter on matrix
#'
#' The function filters out the rows or columns of a matrix such 
#' that the size of the group is exclusively between given min 
#' and max values 
#' 
#' @param network numeric matrix 
#' @param flag numeric 1 for row filtering, 2 for column filtering 
#' @param min numeric value 
#' @param max numeric value 
#' @param ids array to filter on
#'
#' @return network numeric matrix 
#' 
#' @keywords network
#' filter
#' rows 
#'
#' @examples
#' net <- matrix( rnorm(10000), nrow=100)
#' filt_net <- filter_network(net,1,10,100)
#'
#' @export
#'
filter_network <- function(network, flag = 1, min = 0, max = 1, ids = NA) {
    if (flag == 1) {
        network <- filter_network_rows(network, min, max, ids)
    } else if (flag == 2) {
        network <- filter_network_cols(network, min, max, ids)
    }
    
    return(network)
} 
