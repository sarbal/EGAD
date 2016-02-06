#' Fmeasure of precision-recall
#'
#' The function calculates fmeasure for a given beta of a precision-recall curve 
#' 
#' @param recall numeric array
#' @param precis numeric array
#' @param beta numeric value, default is 1 
#'
#' @return fmeasure Numeric value 
#' 
#' @keywords precision-recall fmeasure
#'
#'
#' @export
#'
fmeasure <- function(recall, precis, beta = 1) {
    return(((beta^2 + 1) * precis * recall)/(beta^2 * precis + recall))
} 
