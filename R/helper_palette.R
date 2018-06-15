#use this function for generating the color palette (from colorspace package)
#' Color palette for the MDS
#' 
#' @inheritParams colorspace::heat_hcl
#' 
#' @details Generated with \code{colorspace::choose_palette()}
#'
#' @return \code{n} hex codes from the color palette
#' 
#' @family helper functions
#'
helper_palette <- function(n, h = c(-100, 100), c. = c(60, 100), l = c(15, 95), 
                           power = c(2, 0.9), fixup = TRUE, gamma = NULL, alpha = 1, 
                           ...) {
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- colorspace::hex(colorspace::polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
                       C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
                         rval), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}
