#' Perform row sum
#'
#' @param x an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame
#' @param allNA0 logical indicating that if a whole row is NA to give the row sum as NA. Only works for two dimensional \code{x} and if \code{na.rm} is \code{TRUE}.
#' @param ... other arguments to pass to \code{base::rowSums()}
#'
#' @return A numeric or complex array of suitable size, or a vector if the result is one-dimensional.
#' 
#' @details Essentially equivalent to \code{base::rowSums()} except with the addition of the \code{allNA0} argument.
#' @export
#'
#' @examples
#' x <- data.frame(v1 = c(NA,1:4), v2 = c(NA, 2:5), v3 = c(NA, 1:2, NA, 3))
#' helper_rowSums(x, na.rm = TRUE, allNA0 = TRUE)
#' helper_rowSums(x, na.rm = TRUE, allNA0 = FALSE)
helper_rowSums <- function (x, allNA0 = TRUE, ...) 
{
  
  #store arguments to pass to base::rowSums
  dots <- list(...)
  
  #perform rowSums
  if (length(dots)== 0) out <- base::rowSums(x = x)
  else out <- base::rowSums(x = x, ...)
  
  #if whole row is NA, make rowSum NA (only worry about 2 dimensional x)
  if (length(dim(x))==2L & allNA0) {
    out[apply(x, 1, function(r) all(is.na(r)))] <- NA
  }
  
  return(out)

}
