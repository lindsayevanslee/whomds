#' Check installation of whomds is the most updated
#'
#' @description Compares build date of installed package against the date of the last commit from GitHub (\url{https://github.com/lindsayevanslee/whomds})
#'
#' @return Prints a message stating whether or not installed package is same as most updated version from Github
helper_installation <- function() {
  
  #check packages httr and lubridate are installed
  rlang::check_installed("httr", reason = "to use helper_installation()")
  rlang::check_installed("lubridate", reason = "to use helper_installation()")
  
  #capture last commit time
  commit <- httr::content(httr::GET(url = "https://api.github.com/repos/lindsayevanslee/whomds/git/refs/heads/master"))
  commit_info <- httr::content(httr::GET(url = commit$object$url))
  
  last_commit_datetime <- lubridate::as_datetime(commit_info$author$date)
  
  #capture date of installation of package
  install_datetime <- utils::packageDescription("whomds", fields = "Built")
  install_datetime <- stringr::str_split(install_datetime, "; ") 
  install_datetime <- base::unlist(install_datetime)
  install_datetime <- base::suppressWarnings(lubridate::as_datetime(install_datetime))
  install_datetime <- stats::na.exclude(install_datetime)
  
  #compare installation and commit dates and change startup message accordingly
  if (last_commit_datetime > install_datetime) {
    
    message("There is a more updated version of the package available on github. You may want to reinstall this package to update it.")
    
  } else {
    
    message("Installed version is most updated.")
  }
  
  
  
}


