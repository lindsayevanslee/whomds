#' Check a vector for particular values
#'
#' @param row a vector of data
#' @param check_type a string indicating the type of check to do on \code{row}. Options are \code{"all_equal"}, \code{"all_not_equal"}, and \code{"any_in"}. A value of \code{"all_equal"} will check that all values in \code{row} equal \code{check_value}. A value of \code{"all_not_equal"} will check that all values of \code{row} are not equal to \code{check_value}. A value of \code{"any_in"} will check if any of the values in \code{check_value} are in \code{row}.
#' @param check_value a vector of values to check against
#' @param na_rm logical indicating whether or not to perform check after removing NAs, passed to argument \code{na.rm} of \code{any()} or \code{all()}. Default is TRUE.
#'
#' @return a logical vector with the result of the check
#'
#' @details If all values of \code{row} are NA, then an NA is returned.
#'
#' @family helper functions
helper_checkrow <- function(row,
                            check_type = c("all_equal", "all_not_equal", "any_in"),
                            check_value,
                            na_rm = TRUE) {
  #check check_type inputted correctly
  check_type <- match.arg(check_type)
  
  #check row and check_value have same mode
  if (mode(row) != mode(check_value))
    stop("row and check_value must be the same mode")
  
  #check check_value has length 1 for check_type == "all_equal" or "all_not_equal"
  if ((check_type %in% c("all_equal", "all_not_equal")) &
      length(check_value) > 1)
    stop("check_value must be length 1")
  
  #apply the check
  if (all(is.na(row))) {
    out <- NA
  } else if (check_type == "all_equal") {
    out <- all(row == check_value, na.rm = na_rm)
  } else if (check_type == "all_not_equal") {
    out <- all(row != check_value, na.rm = na_rm)
  } else if (check_type == "any_in") {
    out <- any(row %in% check_value, na.rm = na_rm)
  }
  
  return(out)
}