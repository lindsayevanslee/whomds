#' Example of WHO Brief Model Disability Survey data
#'
#' An example data set of data from the WHO Brief Model Disability Survey
#'
#' @format A data frame with 3000 rows and 84 variables:
#' \describe{
#'   \item{ID}{identification number}
#'   \item{sex}{sex, Male or Female}
#'   \item{age}{age in years}
#'   \item{age_cat}{age catgory, either 17-39, 40-59, or 60+}
#'   \item{work_cat}{whether respondent is currently working, binary}
#'   \item{edu_cat}{highest level of education attained}
#'   \item{marital_status}{marital status}
#'   \item{ethnicity}{ethnicity}
#'   \item{capacity_cat}{capacity level, as defined by WHO cutoffs of CapacityScore}
#'   \item{performance_cat}{performance level, as definted by WHO cutoffs of PerformanceScorePredicted}
#'   \item{CapacityScore}{metric scale of capacity, from Rasch analysis}
#'   \item{PerformanceScorePredicted}{metric scale of performance, from Rasch analysis and random forest regression}
#'   \item{STRATA}{survey strata}
#'   \item{WGT}{survey weights}
#'   \item{B....}{survey item (see Brief MDS questionnaire)}
#' }
#' @source WHO Disability Programme (leel@@who.int)
"mdstest"
