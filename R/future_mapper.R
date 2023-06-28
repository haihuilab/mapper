#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::map()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::map
#' @importFrom magrittr `%>%`
#'
#' @return parameters
#' @export
#'
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


#' future_mapper_chr
#' @rdname future_mapper_chr
#' @export
future_mapper_chr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_dbl
#' @rdname future_mapper_dbl
#' @export
future_mapper_dbl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_dfc
#' @rdname future_mapper_dfc
#' @export
future_mapper_dfc <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_dfr
#' @rdname future_mapper_dfr
#' @export
future_mapper_dfr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_int
#' @rdname future_mapper_int
#' @export
future_mapper_int <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_lgl
#' @rdname future_mapper_lgl
#' @export
future_mapper_lgl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper_walk
#' @rdname future_mapper_walk
#' @export
future_mapper_walk <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_walk(...)
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

