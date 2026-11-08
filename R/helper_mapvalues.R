#' Replace values in a vector
#'
#' Internal replacement for \code{plyr::mapvalues()}: every element of \code{x}
#' equal to an element of \code{from} is replaced by the corresponding element
#' of \code{to}. Other elements are left unchanged. Factors are relabelled via
#' their levels, so their order is preserved.
#'
#' @param x an atomic vector or factor
#' @param from vector of values to replace
#' @param to vector of replacement values, the same length as \code{from}
#' @param warn_missing logical; if TRUE, a message lists any values of
#'   \code{from} that do not occur in \code{x}
#'
#' @return \code{x} with values replaced. As with \code{plyr::mapvalues()}, the
#'   result takes the common type of \code{x} and \code{to}.
#'
#' @keywords internal
#' @noRd
helper_mapvalues <- function(x, from, to, warn_missing = TRUE) {
  if (length(from) != length(to)) stop("`from` and `to` must have the same length")
  if (length(from) == 0) return(x)
  
  if (is.factor(x)) {
    levels(x) <- helper_mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  
  idx <- match(x, from)
  hit <- !is.na(idx)
  
  if (warn_missing) {
    missing_from <- from[!(from %in% x)]
    if (length(missing_from) > 0) {
      message("The following `from` values were not present in `x`: ", paste(missing_from, collapse = ", "))
    }
  }
  
  x[hit] <- to[idx[hit]]
  x
}

#' Rescale a numeric vector to a new range
#'
#' Internal replacement for \code{scales::rescale()}. Missing and non-finite
#' values are ignored when determining the original range and are returned
#' unchanged.
#'
#' @param x numeric vector
#' @param to numeric vector of length 2 giving the target range
#'
#' @return a numeric vector the same length as \code{x}
#'
#' @keywords internal
#' @noRd
helper_rescale <- function(x, to = c(0, 100)) {
  from <- range(x, na.rm = TRUE, finite = TRUE)
  if (diff(from) == 0) return(ifelse(is.na(x), NA_real_, mean(to)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}
