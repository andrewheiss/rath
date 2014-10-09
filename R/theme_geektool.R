#' theme_geektool
#' 
#' A marginless blank theme for showing graphs on a dark background
#' 
#' @import ggplot2
#' @import grid
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x=mpg, y=wt, colour=as.factor(cyl))) + 
#'   geom_point() + theme_ath()

theme_geektool <- function() {
  ret <- theme(axis.line=element_blank(), axis.text.x=element_blank(),
               axis.text.y=element_blank(), axis.ticks=element_blank(),
               axis.title.x=element_blank(), axis.title.y=element_blank(),
               panel.background=element_rect(fill="transparent", colour = NA), 
               panel.border=element_blank(), plot.margin=unit(c(0,0,-1,-1), "lines"),
               panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
               plot.background=element_rect(fill="transparent", colour = NA),
               legend.position="none")
  ret
}
