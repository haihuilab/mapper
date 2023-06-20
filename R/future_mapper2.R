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
map_list <- c(future_map2,
              future_map2_chr,
              future_map2_dbl,
              future_map2_dfc,
              future_map2_dfr,
              future_map2_int,
              future_map2_lgl,
              future_walk2)

mapper_list <- c("future_mapper2",
                 "future_mapper2_chr",
                 "future_mapper2_dbl",
                 "future_mapper2_dfc",
                 "future_mapper2_dfr",
                 "future_mapper2_int",
                 "future_mapper2_lgl",
                 "future_mapper2_walk")


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
# x <- list(1, 10, 100)
# y <- list(1, 2, 3)
# z <- list(5, 50, 500)
#
# future_mapper2(x, y, ~ .x + .y)


