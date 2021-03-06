% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasch_df_nest.R
\name{rasch_df_nest}
\alias{rasch_df_nest}
\title{Nest data by age group}
\usage{
rasch_df_nest(df, vars_group, vars_id)
}
\arguments{
\item{df}{a data frame of individual survey data, where each row is an individual}

\item{vars_group}{a string with the column name identifying grouping variable}

\item{vars_id}{a string with column name uniquely identifying individuals}
}
\value{
nested tibble with two columns: the age group given by \code{vars_group} and \code{df_split} with the split data
}
\description{
Nest data by age group
}
\seealso{
Other rasch functions: 
\code{\link{rasch_DIF}()},
\code{\link{rasch_drop}()},
\code{\link{rasch_factor}()},
\code{\link{rasch_mds_children}()},
\code{\link{rasch_mds}()},
\code{\link{rasch_model_children}()},
\code{\link{rasch_model}()},
\code{\link{rasch_quality_children_print}()},
\code{\link{rasch_quality_children}()},
\code{\link{rasch_rawscore}()},
\code{\link{rasch_recode}()},
\code{\link{rasch_rescale_children}()},
\code{\link{rasch_rescale}()},
\code{\link{rasch_split_age}()},
\code{\link{rasch_split}()},
\code{\link{rasch_testlet}()}

Other children analysis functions: 
\code{\link{rasch_drop}()},
\code{\link{rasch_mds_children}()},
\code{\link{rasch_model_children}()},
\code{\link{rasch_quality_children_print}()},
\code{\link{rasch_quality_children}()},
\code{\link{rasch_recode}()},
\code{\link{rasch_rescale_children}()},
\code{\link{rasch_split_age}()},
\code{\link{rasch_split}()},
\code{\link{rasch_testlet}()}
}
\concept{children analysis functions}
\concept{rasch functions}
