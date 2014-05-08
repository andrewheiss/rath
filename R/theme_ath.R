#' Custom ggplot2 theme
#' 
#' Provides a modified version of \code{theme_bw()}.
#' 
#' @param base_size Base font size (defaults to 12)
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x=mpg, y=wt, colour=as.factor(cyl))) + 
#'   geom_point() + theme_ath()

theme_ath <- function(base_size=12) {
  ret <- theme_bw(base_size) + 
    theme(axis.title=element_text(vjust=0.2), legend.position="bottom")
  ret
}
