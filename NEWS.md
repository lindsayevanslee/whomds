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
