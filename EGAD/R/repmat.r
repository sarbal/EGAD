#' Rep function for matrices
#'
#' The function generates a matrix by binding the columns and rows 
#' 
#' @param X numeric matrix 
#' @param m numeric value, repeat rows m times  
#' @param n numeric value, repeat columns n times  
#' 
#' 
#' @return list of genes and the expression matrix 
#' 
#' @keywords repmat 
#' repeat matrix 
#' 
#' @examples
#'
#' @export
#'
repmat <- function(X, m, n) {
    mx = dim(X)[1]
    nx = dim(X)[2]
    return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T))
} 
