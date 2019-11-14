#' Preview a list of hex colors
#'
#' Show a plot of colors. Code adapted from Karthik Ram's wesandersonr package
#' (https://github.com/karthik/wesanderson/blob/master/R/colors.R).
#'
#' @param x character vector of colors in hex format
#' @param name string of name to print in the palette; if not given, the
#'   function looks for a name attribute in the object
#' @param ... does nothing for now
#'
#' @export
#' @importFrom graphics rect par image
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' color_preview(c("#0074D9", "#2ECC40", "#FFDC00", "#FF4136"))
#'
#' color_preview(c("#0074D9", "#2ECC40", "#FFDC00", "#FF4136"),
#'               name = "Some colors")
#'
#' color_preview(viridisLite::viridis(6, begin = 0.15, end = 0.85,
#'                                    option = "inferno"))
#' }
color_preview <- function(x, name = NULL, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  if (is.null(name)) {
    label <- attr(x, "name")
  } else {
    label <- name
  }
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = label, cex = 1, family = "sans") 
}
