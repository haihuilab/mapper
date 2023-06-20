#' MAPping in Parallel and Ending parallel mapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2
#' @name map_func
#' @export
library(parallel)
library(purrr)
library(furrr)
library(dplyr)


# Run the removing cache function
# 1. future_mapper:future_map------------------------
map_list <- c(future_pmap,
              future_pmap_chr,
              future_pmap_dbl,
              future_pmap_dfc,
              future_pmap_dfr,
              future_pmap_int,
              future_pmap_lgl,
              future_pwalk)

mapper_list <- c("future_pmapper",
                 "future_pmapper_chr",
                 "future_pmapper_dbl",
                 "future_pmapper_dfc",
                 "future_pmapper_dfr",
                 "future_pmapper_int",
                 "future_pmapper_lgl",
                 "future_pmapper_walk")


# 2. future_mapper_chr:future_map_chr------------------------
map_func <- function(i) {
  # Start multicore
  plan(multisession, workers = detectCores()-2)
  options(future.globals.maxSize = 5000000000)

  inner_func <- i
  output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

  }
  # shut down multicore and clear cache
  plan(sequential)
  gc()

  return(output)
}


func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
# Extract the functions as individual ones
list2env(func_list, envir = .GlobalEnv)


#----------------------------------------------
# Examples
# devtools::install_github("haihuilab/mapper")
# library(mapper)
# library(tidyverse)
# Remove cache when using furrr:map functions
# 1:10 %>%
#   future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_mapper_dbl(mean)


