
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A wrapper of `purrr` and `furrr` packages for easy parallel mapping in R (mapper)
## You just need to change `map` function to `mapper` function (and also other mapping functions) for parallel calculation

<!-- badges: start -->
<!-- badges: end -->

The goal of mapper is to using furrr map functions for parallel calculations and automated removing cache.
## Installation

You can install the development version of mapper like so:

``` r
devtools::install_github("haihuilab/mapper")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
# devtools::install_github("haihuilab/mapper")
library(mapper)

# Compute normal distributions from an atomic vector
# map
1:10 |>
  map(rnorm, n = 10)
# mapper: default cores are total cores minus 2 for parallel calculation, you can change the cores by setting `workers=<num>`
1:10 |>
  mapper(rnorm, n = 10, workers = 2)

# Remove cache when using furrr:map functions
1:10 %>%
  future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
  future_mapper_dbl(mean, workers = 4)

```

