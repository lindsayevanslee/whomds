
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whomds

<!-- badges: start -->

[![Build
Status](https://app.travis-ci.com/lindsayevanslee/whomds.svg?branch=master)](https://app.travis-ci.com/lindsayevanslee/whomds)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/lindsayevanslee/whomds?branch=master&svg=true)](https://ci.appveyor.com/project/lindsayevanslee/whomds)
[![codecov](https://codecov.io/gh/lindsayevanslee/whomds/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lindsayevanslee/whomds)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Introduction

This package provides fit-for-purpose functions for calculating and
presenting the results from the Model Disability Survey, as used by the
World Health Organization.

The Model Disability Survey (MDS) is a World Health Organization (WHO)
general population survey instrument to assess the distribution of
disability within a country or region, grounded in the International
Classification of Functioning, Disability and Health. For more
information, please go to <https://www.who.int/health-topics/disability>

## Installation

It is expected the package will soon be uploaded to CRAN. Once available
there, you can install with:

``` r
install.packages("whomds")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lindsayevanslee/whomds", build_vignettes = TRUE)
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

An extensive guide on using the package is available from a series of
vignettes, in both English and Spanish. You can browse the vignettes by
using:

``` r
browseVignettes("whomds")
```

## Contact

Please contact the package maintainer with any questions or comments.
