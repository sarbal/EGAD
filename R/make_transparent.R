#' Make a color transparent (Taken from an answer on StackOverflow by Nick Sabbe)
#'
#' @param color color number, string or hexidecimal code
#' @param alpha numeric transparency
#'
#' @return someColor rgb
#'
#'
#' @import RColorBrewer grDevices
#' @export
#' 
make_transparent <- function(color, alpha = 100) {
    transparent_color <- col2rgb(color)
    apply(transparent_color, 2, function(x) {
        rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, maxColorValue = 255)
    })
} 
