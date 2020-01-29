#' Example of WHO Model Disability Survey data for adults
#'
#' An dummy data set of data from the WHO Model Disability Survey.
#'
#' @format A tibble with 2500 rows and 90 variables:
#' \describe{
#'   \item{HHID}{household number}
#'   \item{strata}{survey strata}
#'   \item{PSU}{PSU id}
#'   \item{weight}{survey weights}
#'   \item{sex}{sex, Male or Female}
#'   \item{age}{age in years}
#'   \item{age_cat}{age catgory, one of "18-24", "25-39", "40-64", or "64-100"}
#'   \item{work_cat}{whether respondent is currently working, binary}
#'   \item{edu_cat}{highest level of education attained}
#'   \item{F....}{survey item from "Functioning" section}
#'   \item{C....}{survey item from "Capacity" section}
#'   \item{EF....}{survey item from "Environmental factors" section}
#'   \item{disability_score}{metric scale of performance, from Rasch analysis}
#'   \item{disability_cat}{performance level, as definted by WHO cutoffs of disability_score}
#' }
"df_adults"

#' Example of WHO Model Disability Survey data for children
#'
#' An dummy data set of data from the WHO Model Disability Survey. 
#'
#' @format A tibble with 2500 rows and 42 variables:
#' \describe{
#'   \item{HHID}{household number}
#'   \item{strata}{survey strata}
#'   \item{PSU}{PSU id}
#'   \item{weight}{survey weights}
#'   \item{sex}{sex, Male or Female}
#'   \item{age}{age in years}
#'   \item{age_cat}{age catgory, one of "Age2to4", "Age5to9", or "Age10to17"}
#'   \item{child....}{survey item from MDS Children questionnaire"}
#' }
"df_children"