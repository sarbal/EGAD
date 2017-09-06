#' Make a color transparent (Taken from an answer on StackOverflow by Nick Sabbe)
#'
#' @param someColor color number, string or hexidecimal code
#' @param alpha numeric transparency
#'
#' @return someColor rgb
#'
#'
#' @export
#' @import RColorBrewer
#'
make_transparent <- function(someColor, alpha = 100) {
    newColor <- col2rgb(someColor)
    apply(newColor, 2, function(x) {
        rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, maxColorValue = 255)
    })
} 
