# rasch_mds_children() prepares the survey data before calling this function.
# To use the function on its own, do the same preparation by hand:
# 1. list the items: those asked of every child ("common") and those asked
#    only of children in a particular age group
vars_metric <- list(
  common    = paste0("child", 1:10),
  Age2to4   = paste0("child", c(12, 15, 16, 19, 20, 24, 25)),
  Age5to9   = paste0("child", c(11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 25, 27)),
  Age10to17 = paste0("child", c(11, 13, 14, 17, 18, 20, 21, 22, 23, 25, 26, 27))
)
items <- unique(unlist(vars_metric))
df <- df_children
# 2. make the ID column text
df$HHID <- as.character(df$HHID)
# 3. shift the responses 1 to 5 down to 0 to 4; any other code (such as 88
#    for "don't know") becomes missing
df[items] <- lapply(df[items], function(x) ifelse(x %in% 1:5, x - 1, NA))
# 4. keep only children with at least one answer of 4 or 5 (3 or 4 after the
#    shift), as rasch_mds_children() does by default (see has_at_least_one)
df <- df[rowSums(df[items] >= 3, na.rm = TRUE) > 0, ]
# 5. record the highest possible value of each item
max_values <- data.frame(var = items, max_val = 4)
