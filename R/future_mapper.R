#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::map()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::map
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#' future_mapper_template
#' @export


#' future_mapper
#' @rdname future_mapper
#' @export
future_mapper <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)

  # map function
  res <- furrr::future_map(...)

  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()

  return(res)
}



# Example
# library(tidyverse)
# library(furrr)
# # Remove cache when using furrr:map functions
# 1:10 %>%
#   future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

