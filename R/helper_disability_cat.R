#' Categorize a disability score into levels of disability
#'
#' Cuts a disability score, typically the \code{rescaled} score from
#' \code{rasch_mds()} or \code{rasch_mds_children()} ranging from 0 to 100,
#' into ordered levels of disability.
#'
#' @param score a numeric vector of scores
#' @param cutoffs a numeric vector of strictly increasing cut-offs, with one
#'   fewer element than \code{labels}. The default \code{NULL} uses the mean
#'   minus one standard deviation, the mean, and the mean plus one standard
#'   deviation of \code{score} (ignoring missing values), which requires four
#'   \code{labels}.
#' @param labels a character vector of level names in increasing order of
#'   disability. Default is \code{c("No", "Mild", "Moderate", "Severe")}.
#'
#' @details Each level includes its lower cut-off and excludes its upper one,
#'   so a score equal to a cut-off is assigned to the higher level. A score of
#'   exactly 0 is always assigned to the first level, even when the lowest
#'   cut-off is 0 or below. Missing scores give missing levels.
#'
#'   With the default cut-offs the levels are defined relative to the
#'   population surveyed. This is how \code{disability_cat} in the example data
#'   \code{df_adults} was created. Supply \code{cutoffs} explicitly to keep
#'   levels comparable across surveys.
#'
#' @return an ordered factor the same length as \code{score} with levels
#'   \code{labels}. The cut-offs used are stored in the attribute
#'   \code{"cutoffs"}, so they can be retrieved with
#'   \code{attr(x, "cutoffs")} and passed to, for example, \code{fig_dist()}.
#'
#' @family helper functions
#'
#' @export
#'
#' @examples
#' disability_cat <- helper_disability_cat(df_adults$disability_score)
#' table(disability_cat)
#' attr(disability_cat, "cutoffs")
#'
#' # fixed cut-offs and three levels
#' helper_disability_cat(c(0, 10, 50, 90), cutoffs = c(20, 60),
#'                       labels = c("Low", "Medium", "High"))
helper_disability_cat <- function(score, cutoffs = NULL, labels = c("No", "Mild", "Moderate", "Severe")) {
  
  if (!is.numeric(score)) stop("`score` must be numeric")
  if (length(labels) < 2 || anyDuplicated(labels)) stop("`labels` must be at least two distinct values")
  
  if (is.null(cutoffs)) {
    if (length(labels) != 4) stop("The default cut-offs (mean - SD, mean, mean + SD) define four levels; supply `cutoffs` to use a different number of `labels`")
    m <- mean(score, na.rm = TRUE)
    s <- stats::sd(score, na.rm = TRUE)
    cutoffs <- c(m - s, m, m + s)
    if (anyNA(cutoffs)) stop("Could not compute default cut-offs: `score` needs at least two non-missing values")
  }
  
  if (!is.numeric(cutoffs) || length(cutoffs) != length(labels) - 1) stop("`cutoffs` must be numeric with one fewer element than `labels`")
  if (anyNA(cutoffs) || is.unsorted(cutoffs, strictly = TRUE)) stop("`cutoffs` must be strictly increasing with no missing values")
  
  out <- cut(score,
             breaks = c(-Inf, cutoffs, Inf),
             labels = labels,
             right = FALSE,
             ordered_result = TRUE)
  
  # a score of exactly 0 means no disability regardless of the cut-offs
  out[!is.na(score) & score == 0] <- labels[1]
  
  attr(out, "cutoffs") <- unname(cutoffs)
  out
}
