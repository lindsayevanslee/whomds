
# rasch_mds() also removes people with too many missing answers, here more
# than 2 of the 12 items. The model takes about 10 seconds on all 2500
# adults, so this example uses only the first 300 who remain
n_missing <- rowSums(is.na(df[items]))
df <- df[n_missing <= 2, ]
df <- df[1:300, ]
