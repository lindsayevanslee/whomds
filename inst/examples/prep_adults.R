# rasch_mds() prepares the survey data before calling this function. To use
# the function on its own, do the same preparation by hand:
# 1. choose the items (survey questions) that will make up the scale
items <- paste0("EF", 1:12)
# 2. shift the responses 1 to 5 down to 0 to 4, as the Rasch Model expects
df <- df_adults
df[items] <- df[items] - 1
# 3. make the ID column text, so that the artificial IDs "MIN" and "MAX" can
#    be added to it later
df$HHID <- as.character(df$HHID)
# 4. record the highest possible value of each item (5, or 4 after the shift)
max_values <- data.frame(var = items, max_val = 4)
