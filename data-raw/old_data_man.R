######### manual information for old data files used from Chile implementation

# # #' Example of WHO Model Disability Survey data for adults from Costa Rica
# # #'
# # #' An example data set of data from the WHO Model Disability Survey. The data is taken from the Costa Rica survey ENADIS (2018)
# # #'
# # #' For full details about the data, including variable descriptions, please go to \url{http://sistemas.inec.cr/pad4/index.php/catalog/218}
# # #'
# # #' @source Instituto Nacional de Estadística y Censos (INEC) de Costa Rica, WHO Disability Programme (leel@@who.int)
# "costa_rica"
# 
# # #' Example of WHO Model Disability Survey data for adults from Chile
# # #'
# # #' An example data set of data from the WHO Model Disability Survey. The data is taken from the Chilean survey ENDIS II (2014)
# # #'
# # #' @format A tibble with 12256 rows and 136 variables:
# #' \describe{
# #'   \item{enc_id}{household number}
# #'   \item{sex}{sex, Male or Female}
# #'   \item{edad}{age in years}
# #'   \item{age_cat}{age catgory, either "18-39y", "40-59y", or "60+y"}
# #'   \item{work_cat}{whether respondent is currently working, binary}
# #'   \item{edu_cat}{highest level of education attained}
# #'   \item{marital_status}{marital status}
# #'   \item{capacity_cat}{capacity level, as defined by WHO cutoffs of CapacityScore}
# #'   \item{performance_cat}{performance level, as definted by WHO cutoffs of PerformanceScorePredicted}
# #'   \item{CapacityScore}{metric scale of capacity, from Rasch analysis}
# #'   \item{PerformanceScorePredicted}{metric scale of performance, from Rasch analysis and random forest regression}
# #'   \item{VARUNIT_N}{cluster id}
# #'   \item{VARSTRAT_N}{survey strata}
# #'   \item{Factor_Persona}{survey weights}
# #'   \item{c....}{survey item from "CAPACIDAD" section (see Chile ENDIS II questionnaire)}
# #'   \item{d....}{survey item from "DESEMPENO" section (see Chile ENDIS II questionnaire)}
# #'   \item{f....}{survey item from "FACTORES AMBIENTALES" section (see Chile ENDIS II questionnaire)}
# #' }
# #' @source WHO Disability Programme (leel@@who.int), El Servicio Nacional de la Discapacidad de Chile
# "chile_adults"
# 
# #' Example of WHO Model Disability Survey data for children from Chile
# #'
# #' An example data set of data from the WHO Model Disability Survey. The data is taken from the Chilean survey ENDIS II (2014)
# #'
# #' @format A tibble with 5515 rows and 199 variables:
# #' \describe{
# #'   \item{enc_id}{household number}
# #'   \item{sex}{sex, Male or Female}
# #'   \item{edad}{age in years}
# #'   \item{age_cat}{age catgory, either "Age2to4",  "Age5to9" or "Age10to17"}
# #'   \item{VARUNIT_N}{cluster id}
# #'   \item{VARSTRAT_N}{survey strata}
# #'   \item{Factor_Persona}{survey weights}
# #'   \item{e....}{survey item from "EDUCACION" section (see Chile ENDIS II questionnaire)}
# #'   \item{n....}{survey item from "CUESTIONARIO INFANTIL" (see Chile ENDIS II questionnaire)}
# #' }
# #' @source WHO Disability Programme (leel@@who.int), El Servicio Nacional de la Discapacidad de Chile
# "chile_children"