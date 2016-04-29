#' Plot network heatmap
#'
#' The function draws a heatmap to visualize a network
#'
#' @param net a numeric matrix of edge weights
#' @param colrs a range of colors to plot the network 
#'
#' @keywords
#' plot  heatmap image network
#'
#' @return null 
#'
#' @examples 
#' network <- cor(matrix( rnorm(10000), nrow=100))
#' plot_network_heatmap(network)
#'
#' @export
#' @import gplots
#'
plot_network_heatmap <- function(net, colrs) {
    
    if (missing(colrs)) {
        colrs = colorpanel(100, "red", "blue")
    }
    nd = node_degree(net)
    o = order(nd)
    image(net[o, o], axes = FALSE, bty = "n", col = colrs)
    
} 
