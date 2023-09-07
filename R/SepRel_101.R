#' Separation Reliability: Person Separation Reliability
#'
#' Copied from eRm::SepRel(), v1.0-1: This function calculates the proportion of person variance that is not due to error. The concept of person separation reliability is very similar to reliability indices such as Cronbach's alpha.
#'
#' @param pobject Object of class \code{ppar} (see \code{?eRm::person.parameter}).
#'
#' @return SepRel returns a list object of class eRm_SepRel containing:
#' \item{sep.rel}{the person separation reliability,}
#' \item{SSD.PS}{the squared standard deviation (i.e., total person variability),}
#' \item{MSE}{the mean square measurement error (i.e., model error variance).}
#' 
#' @details 
#' See full documentation at \url{https://www.rdocumentation.org/packages/eRm/versions/1.0-1/topics/Separation\%20Reliability}
#'  
#' @author Original code by Adrian Brügger (\email{Adrian.Bruegger@imu.unibe.ch}), adapted by Marco J. Maier in package \code{eRm} v1.0-1
#' 
#' @references 
#' Wright, B.D., and Stone, M.H. (1999). Measurement essentials. Wide Range Inc., Wilmington. (\url{https://www.rasch.org/measess/me-all.pdf} 28Mb).
#' 
#' 
#' @export
SepRel_1.0.1 <- function (pobject) 
{
  if (!("ppar" %in% class(pobject))) 
    stop("\"pobject\" must be of class \"ppar\"")
  PersonScoresFull <- pobject[["theta.table"]][["Person Parameter"]]
  PersonScores <- PersonScoresFull[stats::complete.cases(PersonScoresFull)]
  StandardErrors <- unlist(pobject[["se.theta"]])
  SSD.PersonScores <- var(PersonScores)
  MSE <- sum((StandardErrors)^2)/length(StandardErrors)
  separation.reliability <- (SSD.PersonScores - MSE)/SSD.PersonScores
  result <- structure(list(sep.rel = separation.reliability, 
                           SSD.PS = SSD.PersonScores, MSE = MSE), class = "eRm_SepRel")
  return(result)
}
