# whomds (development version)

- New function `helper_disability_cat()` categorizes a disability score into ordered levels, by default No, Mild, Moderate and Severe with cut-offs at the mean minus one SD, the mean and the mean plus one SD of the score, which is how `disability_cat` in `df_adults` was created. Explicit cut-offs and labels can be supplied. Documented in the `c6_after_rasch` vignette (#10)
- `rasch_mds()` and `rasch_mds_children()` now also return the standard error of each person's ability estimate in a new column `person_pars_se`, next to `person_pars`, in the returned data and in `Data_final.csv` (#14). It is on the same logit scale as `person_pars`. This requires eRm >= 1.0-7
- Fixed error in `rasch_mds()` / `rasch_model()` ("Can't use NA as column index in a tibble for assignment") caused by eRm >= 1.0-7 adding a `Std.Error` column to `person.parameter()$theta.table`. The person ability columns in `df_score` are now selected by name instead of by position, so `NAgroup` and `Interpolated` are labeled correctly again
- Fixed tidyselect deprecation warnings in `rasch_recode()`, `rasch_rawscore()`, `rasch_DIF()`, `rasch_split_age()` and `rasch_rescale()`, and replaced a deprecated matrix-to-tibble conversion in `rasch_rawscore()`
- Fixed `table_basicstats()` giving wrong results when `group_by_var` is a character variable rather than a factor: households with no members in a group were omitted instead of being counted as 0
- Removed a `na.rm` argument in `table_weightedpct()` that had no effect on grouped proportions and triggered a warning from srvyr
- Fixed `fig_LID()` failing with "adjacency matrix contains NAs" when two items have no respondents in common (for example items split by age group in `rasch_mds_children()`, or by `split_strategy` in `rasch_mds()`), which broke `print_results = TRUE`. Such item pairs are now treated as having no local dependence
- Fixed remaining deprecation warnings: `as_tibble()` on unnamed matrices and bare column selections in `rasch_DIF()` and `rasch_split_age()`, the `size` aesthetic for lines in `fig_density()` (now `linewidth`, requiring ggplot2 >= 3.4.0), and `aes_string()` in `fig_dist()`. Also fixed a spurious "no non-missing arguments to min" warning from `fig_density()` when `cutoffs` is NULL
- Removed dependencies on plyr, readr, scales and stringr, replacing them with base R equivalents and small internal helpers. Output CSV files are now written with `utils::write.csv()`, so character values are quoted
- Added unit tests for the data-preparation functions, input validation in `rasch_mds()`, and the helper functions

# whomds 1.1.1

- Fixed bug with related to printing of DIF results
- Create new function SepRel_101() which is a copy of eRm::SepRel() v1.0-1
- Fixed bug with anchored model output in rasch_quality_children_print()
- Updated use of max_NA in rasch_mds_children() so that the function only looks for missing values in the items relevant to the respondent's group
- Fixed documentation issue with whomds-package alias as requested by CRAN

# whomds 1.1.0

- Reduced number of packages in Imports by moving some to Suggests and removing others
- Updated version of R required to v3.6.0 or newer
- Fixed some typos in documentation (#7)
- Fixed labels in output of `fig_LID()` (#5)
- Fixed setting of x-axis labels in `fig_density()` (#2)
- Changed some syntax that had been deprecated or superseded in tidyverse functions (#6, #11)
- Updated broken URLs
- Updated example used in documentation for `table_basicstats()`

# whomds 1.0.1

- Updated DESCRIPTION to refer to license correctly
- Updated README and vignettes to refer to installation of package from CRAN

# whomds 1.0.0

- First release on CRAN! 🎉
