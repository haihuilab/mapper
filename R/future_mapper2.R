#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::map2()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::map2
#' @importFrom magrittr `%>%`
#'
#' @return parameters
#' @export
#'
#' future_mapper2
#' @rdname future_mapper2
#' @export
future_mapper2 <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_chr
#' @rdname future_mapper2_chr
#' @export
future_mapper2_chr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_dbl
#' @rdname future_mapper2_dbl
#' @export
future_mapper2_dbl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_dfc
#' @rdname future_mapper2_dfc
#' @export
future_mapper2_dfc <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_dfr
#' @rdname future_mapper2_dfr
#' @export
future_mapper2_dfr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_int
#' @rdname future_mapper2_int
#' @export
future_mapper2_int <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_lgl
#' @rdname future_mapper2_lgl
#' @export
future_mapper2_lgl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_map2_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_mapper2_walk2
#' @rdname future_mapper2_walk2
#' @export
future_mapper2_walk2 <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_walk2(...)
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

