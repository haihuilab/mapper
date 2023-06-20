#' MAPping in Parallel and Ending parallel mapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2
#' @name future_mapper
#' @export
library(parallel)
library(purrr)
library(furrr)
library(dplyr)


# Run the removing cache function
# 1. future_mapper:future_map------------------------
map_list <- c(future_map,
              future_map_chr,
              future_map_dbl,
              future_map_dfc,
              future_map_dfr,
              future_map_int,
              future_map_lgl,
              future_walk)

mapper_list <- c("future_mapper",
                 "future_mapper_chr",
                 "future_mapper_dbl",
                 "future_mapper_dfc",
                 "future_mapper_dfr",
                 "future_mapper_int",
                 "future_mapper_lgl",
                 "future_mapper_walk")


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


