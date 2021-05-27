
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wheelchair\_cdap2

<!-- badges: start -->
<!-- badges: end -->

This repository contains data analysis and manuscript files related to a
study on how overall daily activity patterns are affected by the use of
a wheelchair.

## Analysis

The analysis can be executed with the `targets` package. To do this,
clone the repository, install `targets`, and then run the following
commands:

``` r
library(targets)
tar_make()
#> [32m✓[39m skip target hh
#> [32m✓[39m skip target dummy
#> [32m✓[39m skip target persons
#> [32m✓[39m skip target person_dap
#> [32m✓[39m skip target data
#> [32m✓[39m skip pipeline
```

The analysis uses several R packages. Users can install the necessary
packages by doing the following:

``` r
# install.packages("devtools") # devtools: install non-CRAN pkgs on GitHub
devtools::install_github("byu-transpolab/apollo") # personal fork of apollo
devtools::install_github("byu-transpolab/nhts2017") # NHTS 2017 tables

install.packages("mlogit")
install.packages("tidyverse")
```

## Document

The document is written in `bookdown` using the targets created above.
