#' Build precision-recall curve
#'
#' The function calculates the recall and precision 
#' 
#' @param ranks numeric array
#' @param labels binary array
#'
#' @return recall,precision numeric arrays  
#'
#' @keywords precsion-recall
#' precision
#' recall
#'
#' @examples
#' labels <- c(rep(0,10))
#' labels[c(1,3,5)] <- 1 
#' scores <- 10:1
#' ranks <- rank(scores)
#' prc <- get_prc(ranks, labels)
#'
#' @export
#'
get_prc <- function(ranks, labels) {
    
    # Get the ranks/positions of the positive scores
    positives <- which(labels == 1, arr.ind = TRUE)
    
    n <- length(positives)
    
    o <- order(ranks, decreasing = TRUE)
    
    fp <- cumsum(!labels[o])
    tp <- cumsum(labels[o])
    fn <- n - tp
    
    precis <- tp/(tp + fp)
    recall <- tp/n
    
    return(cbind(recall, precis))
} 
