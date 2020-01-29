
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whomds

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/lindsayevanslee/whomds.svg?branch=master)](https://travis-ci.com/lindsayevanslee/whomds)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/lindsayevanslee/whomds?branch=master&svg=true)](https://ci.appveyor.com/project/lindsayevanslee/whomds)
[![codecov](https://codecov.io/gh/lindsayevanslee/whomds/branch/master/graph/badge.svg)](https://codecov.io/gh/lindsayevanslee/whomds)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Introduction

This package provides fit-for-purpose functions for calculating and
presenting the results from the Model Disability Survey, as used by the
World Health Organization.

The Model Disability Survey (MDS) is a World Health Organization (WHO)
general population survey instrument to assess the distribution of
disability within a country or region, grounded in the International
Classification of Functioning, Disability and Health. For more
information, please go to <http://www.who.int/disabilities/data/mds/en/>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lindsayevanslee/whomds")
```

## Example

A Rasch model can be used with the function `rasch_mds()`, like so:

``` r
library(whomds)

start <- rasch_mds(
  df = df_adults, 
  vars_metric = paste0("EF", 1:12),
  vars_id = "HHID", 
  vars_DIF = c("sex", "age_cat"),
  resp_opts = 1:5, 
  max_NA = 2,
  print_results = TRUE,
  path_parent = "/Users/lindsaylee/Desktop/",
  model_name = "Start",
  testlet_strategy = NULL,
  recode_strategy = NULL,
  drop_vars = NULL,
  split_strategy = NULL,
  comment = "Initial run"
)
```

## More information

An extensive guide on using the package is available. Please email me at
<leel@who.int> for access. Some of the information is already included
in vignettes for this package. In the future, all information in the
guide will be included as vignettes.

The package is still very much in development mode, so there is much
work that needs to be done. If you notice any bugs or have any
suggestions, please email me or submit an issue here on GitHub.

### Who am I

I am a technical officer in the Disability Programme at the WHO and
responsible for work related to the MDS. Please email me at
<leel@who.int> if you have questions or comments.
