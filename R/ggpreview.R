#' Preview output from \code{ggsave()}
#'
#' Displays a preview of what a ggplot-based plot looks like after being saved
#' with \code{\link{ggsave}}. Avoids the hassle of exporting a plot, switching
#' to your system file explorer, checking the output, and returning to R to make
#' more adjustments.
#'
#' The heavy lifting here came from
#' \href{https://twitter.com/tjmahr/status/1083094031826124800?s=12}{TJ Mahr}. I
#' added the Cairo option because the Cairo graphics library can (1) properly
#' embed fonts in PDFs, and (2) create PNGs with the correct DPI and dimensions.
#' See
#' \href{https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/}{this
#' blog post} for more details.
#'
#' @param device name of device to be used in \code{\link{ggsave}} (defaults to
#'   "png")
#' @param cairo logical indicator marking if Cairo graphics should be used
#'   (defaults to FALSE)
#' @param ... additional options passed to \code{\link{ggsave}}
#' @import ggplot2
#' @importFrom grDevices cairo_pdf
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Generic example plot
#' plot1 <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point()
#'
#' # Plot with custom fonts (for Cairo)
#' plot2 <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point() +
#'   theme_grey(base_family = "Times New Roman")
#'
#' # Preview with default dimensions
#' ggpreview(plot1)
#'
#' # Preview with specific dimensions
#' ggpreview(plot1, width = 7, height = 4)
#'
#' # Preview as PDF
#' ggpreview(plot1, device = "pdf", width = 7, height = 4)
#'
#' # Preview as Cairo PDF
#' ggpreview(plot2, device = "pdf", cairo = TRUE,
#'           width = 7, height = 4)
#'
#' # Preview as high-resolution Cairo PNG
#' ggpreview(plot2, device = "png", cairo = TRUE,
#'           width = 7, height = 4, dpi = 300)
#' }

ggpreview <- function(..., device = "png", cairo = FALSE) {
  fname <- tempfile(fileext = paste0(".", device))
  
  if (cairo & device == "pdf") {
    ggplot2::ggsave(filename = fname, device = cairo_pdf, ...)
  } else if (cairo & device == "png") {
    ggplot2::ggsave(filename = fname, device = device, type = "cairo", ...)
  } else {
    ggplot2::ggsave(filename = fname, device = device, ...)
  }
  
  system2("open", fname)
  invisible(NULL)
}
