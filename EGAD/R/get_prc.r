#' Build precision-recall curve
#'
#' The function calculates the recall and precision 
#' 
#' @param scores numeric array
#' @param labels binary array
#'
#' @return recall,precision numeric arrays  
#'
#' @keywords precsion-recall
#' precision
#' recall
#'
#' @export
#'
get_prc <- function(scores, labels) {

    # Get the ranks/positions of the positive scores
    positives <- which(labels == 1, arr.ind = T)
    
    n <- length(positives)
    
    o <- order(scores, decreasing = T)
    
    fp <- cumsum(!labels[o])
    tp <- cumsum(labels[o])
    fn <- n - tp
    
    precis <- tp/(tp + fp)
    recall <- tp/n
    
    return(cbind(recall, precis))
} 
