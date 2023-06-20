
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MAPping in Parallel and Ending parallel mapping `furrr` in R (mapper)

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
library(tidyverse)
Remove cache when using furrr:map functions
1:10 %>%
  future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
  future_mapper_dbl(mean)

```

