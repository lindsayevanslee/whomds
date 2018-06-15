x <- rasch_mds(
  df = dplyr::sample_n(mdstest, 2000),
  vars_metric = paste0("fa", 1:12),
  vars_id = "enc_id",
  vars_DIF = "sex",
  resp_opts = 1:5,
  max_NA = 2,
  testlet_strategy = NULL,
  recode_strategy = NULL,
  drop_vars = NULL,
  split_strategy = NULL,
  comment = "Testing this!!!!",
  print_results = TRUE,
  model_name = "Test_Final",
  path_parent = "~/Desktop/"
)

library(tidyverse)
library(TAM)
df = read_csv(paste0("/Users/lindsaylee/Dropbox/WHO/MDS/09 national and regional surveys/Sri Lanka 2015/01 Data/srilanka_children.csv")) %>% 
  mutate(age_cat = cut(
    H1009,
    breaks = c(2, 5, 10, 17),
    labels = c("Age2to4", "Age5to9", "Age10to17"),
    right = FALSE,
    include.lowest = TRUE,
    ordered_result = TRUE
  ))
vars_age_group = "age_cat"
vars_metric_common = NULL
vars_metric_grouped = NULL
vars_metric = list(common=paste0("M",c(10009, 10011:10015, 10017, 10027, 10032)),
                   "Age2to4"=paste0("M",c(10019, 10023, 10026, 10031)),
                   "Age5to9"=paste0("M",c(10018, 10020, 10025, 10029, 10030, 10031, 10034)),
                   "Age10to17"=paste0("M",c(10018, 10020, 10025, 10029, 10030, 10033, 10034)))
vars_id = "MDS_ID"
vars_DIF = NULL
resp_opts = 1:5
has_at_least_one = 4:5
max_NA = 10
TAM_model = "PCM2"
testlet_strategy = list(t1 = c("M10009", "M10011"),
                        t2 = c("M10019", "M10023"))
recode_strategy = NULL
drop_vars = NULL
split_strategy = NULL
comment = "INITIAL CHILDREN TEST"
print_results = TRUE
model_name = "INITIAL_CHILDREN_TEST"
path_parent = "~/Desktop/"


