.onAttach <- function(libname, pkgname) {
  #capture last commit time
  commit <- httr::content(httr::GET("https://api.github.com/repos/lindsayevanslee/whomds/git/refs/heads/master"))
  commit_info <- httr::content(httr::GET(commit$object$url))
  
  last_commit_datetime <- lubridate::as_datetime(commit_info$author$date)
  
  #capture date of installation of package
  install_datetime <- utils::packageDescription("whomds", fields = "Built")
  install_datetime <- stringr::str_split(install_datetime, "; ") 
  install_datetime <- base::unlist(install_datetime)
  install_datetime <- lubridate::as_datetime(install_datetime)
  install_datetime <- stats::na.exclude(install_datetime)
  
  #compare installation and commit dates and change startup message accordingly
  if (last_commit_datetime > install_datetime) {
    
    packageStartupMessage("Thank you for using whomds. To learn more about the WHO Model Disability Survey (MDS) and for contact information, go to https://www.who.int/disabilities/data/en/ \n \n NOTE: There is a more updated version of the package available on github. You may want to reinstall this package to update it.")
    
  } else {
    
    packageStartupMessage("Thank you for using whomds. To learn more about the WHO Model Disability Survey (MDS) and for contact information, go to https://www.who.int/disabilities/data/en/")
    
  }
  
  
}