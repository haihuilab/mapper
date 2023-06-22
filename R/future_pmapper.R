#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::pmap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::pmap
#' @importFrom magrittr `%>%`
#'
#' @return parameters
#' @export
#'
#' future_pmapper
#' @rdname future_pmapper
#' @export
future_pmapper <- function(...) {
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


#' future_pmapper_chr
#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- function(...) {
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


#' future_pmapper_dbl
#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- function(...) {
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


#' future_pmapper_dfc
#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- function(...) {
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


#' future_pmapper_dfr
#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- function(...) {
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


#' future_pmapper_int
#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- function(...) {
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


#' future_pmapper_lgl
#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- function(...) {
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


#' future_pmapper_pwalk
#' @rdname future_pmapper_wpalk
#' @export
future_pmapper_pwalk <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_pwalk(...)
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
#   future_pmapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

