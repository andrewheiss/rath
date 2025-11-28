#' Convert picas to numeric
#'
#' @param picas numeric value in picas, or character string in InDesign format
#'   (e.g., "1p6" for 1 pica and 6 points)
#'
#' @return Numeric value in picas
#' @keywords internal
#'
#' @examples
#' picas_to_num(1.5)
#' picas_to_num("1p6")
#' picas_to_num("1p")
picas_to_num <- function(picas) {
  if (is.numeric(picas)) {
    return(picas)
  }

  parts <- strsplit(picas, "p")[[1]]
  pica_part <- as.numeric(parts[1])
  point_part <- if (length(parts) > 1 && parts[2] != "") {
    as.numeric(parts[2]) / 12
  } else {
    0
  }

  pica_part + point_part
}


#' Clean decimal formatting
#'
#' Remove trailing zeros and decimal points from numeric values formatted as
#' strings. Used internally to format point values in pica notation.
#'
#' @param x numeric value to format
#'
#' @return Character string with trailing zeros and decimal point removed
#' @keywords internal
#'
#' @examples
#' clean_decimal(2.3)
#' clean_decimal(2.0)
#' clean_decimal(2.30000000000002)
clean_decimal <- function(x) {
  x <- sprintf("%.10f", x)
  x <- sub("0+$", "", x)
  x <- sub("\\.$", "", x)
  x
}

#' Convert picas to points
#'
#' @param picas numeric value in picas, or character string in InDesign format
#'   (e.g., "1p6" for 1 pica and 6 points)
#'
#' @return Numeric value in points (72 points = 1 inch)
#' @export
#'
#' @examples
#' picas_to_pts(1.5)
#' picas_to_pts("1p6")
picas_to_pts <- function(picas) {
  picas_to_num(picas) * 12
}

#' Convert points to picas
#'
#' @param points numeric value in points (72 points = 1 inch)
#' @param numeric logical; if TRUE, return numeric picas. If FALSE (default),
#'   return InDesign-style string (e.g., "1p6")
#'
#' @return Picas as numeric or InDesign-style string
#' @export
#'
#' @examples
#' pts_to_picas(18)
#' pts_to_picas(18, numeric = TRUE)
#' pts_to_picas(182.3)
pts_to_picas <- function(points, numeric = FALSE) {
  picas <- points / 12

  if (numeric) {
    return(picas)
  }

  pica_part <- floor(picas)
  point_part <- (picas - pica_part) * 12

  paste0(pica_part, "p", clean_decimal(point_part))
}

#' Convert picas to inches
#'
#' @param picas Numeric value in picas, or character string in InDesign format
#'   (e.g., "1p6" for 1 pica and 6 points)
#'
#' @return Numeric value in inches
#' @export
#'
#' @examples
#' picas_to_in(1.5)
#' picas_to_in("1p6")
picas_to_in <- function(picas) {
  picas_to_num(picas) / 6
}

#' Convert inches to picas
#'
#' @param inches numeric value in inches
#' @param numeric logical; if TRUE, return numeric picas. If FALSE (default),
#'   return InDesign-style string (e.g., "1p6")
#'
#' @return Picas as numeric or InDesign-style string
#' @export
#'
#' @examples
#' in_to_picas(0.25)
#' in_to_picas(0.25, numeric = TRUE)
#' in_to_picas(3.1534)
in_to_picas <- function(inches, numeric = FALSE) {
  picas <- inches * 6

  if (numeric) {
    return(picas)
  }

  pica_part <- floor(picas)
  point_part <- (picas - pica_part) * 12

  paste0(pica_part, "p", clean_decimal(point_part))
}
