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
#' pmapper
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper
#' @export
pmapper <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_chr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_chr
#' @export
pmapper_chr <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_dbl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_dbl
#' @export
pmapper_dbl <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_dfc
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_dfc
#' @export
pmapper_dfc <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_dfr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_dfr
#' @export
pmapper_dfr <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_int
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_int
#' @export
pmapper_int <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_lgl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_lgl
#' @export
pmapper_lgl <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_pmap_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' pmapper_pwalk
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname pmapper_wpalk
#' @export
pmapper_pwalk <- function(workers = (parallelly::availableCores()-2), ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
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
#   pmapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

