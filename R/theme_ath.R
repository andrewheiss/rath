#' My custom ggplot theme
#' 
#' Provides a modified version of \code{\link{theme_bw}}.
#' 
#' @param base_size base font size (defaults to 12)
#' @param base_family base font family
#' @param legend_bottom set this to TRUE to place the legend at the bottom of the plot
#' @param margin_bottom bottom plot margin, in lines (\bold{only used when} \code{legend_bottom = TRUE}) (defaults to -0.5)
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x=mpg, y=wt, colour=as.factor(cyl))) + 
#'   geom_point() + theme_ath()
#'   
#' ggplot(mtcars, aes(x=mpg, y=wt, colour=as.factor(cyl))) + 
#'   geom_point() + theme_ath(legend_bottom=TRUE)

theme_ath <- function(base_size=12, base_family="", legend_bottom=FALSE, margin_bottom=-0.5) {
  ret <- theme_bw(base_size, base_family) + 
    theme(axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=0.2))
  
  if (legend_bottom) {
    require(grid)
    ret <- ret + theme(legend.position="bottom", legend.margin=unit(0, "lines"),
            # Negative bottom margin necessary because legend.margin takes only one value
            plot.margin=unit(c(1, 1, margin_bottom, 0.5), "lines"))
  }
  
  ret
}
