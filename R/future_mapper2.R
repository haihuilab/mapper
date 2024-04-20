#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::map2()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map2
#' @inheritParams furrr::map2
#' @importFrom magrittr `%>%`
#'
#' @return parameters
#' @export
#'
#' mapper2
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2
#' @export
mapper2 <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_chr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_chr
#' @export
mapper2_chr <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_dbl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_dbl
#' @export
mapper2_dbl <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_dfc
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_dfc
#' @export
mapper2_dfc <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_dfr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_dfr
#' @export
mapper2_dfr <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_int
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_int
#' @export
mapper2_int <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_lgl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_lgl
#' @export
mapper2_lgl <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

  # map function
  res <- furrr::future_map2_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper2_walk2
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper2_walk2
#' @export
mapper2_walk2 <- function(..., workers = NULL) {

  if (is.null(workers)) {
    workers <- parallelly::availableCores(omit = 2)
  }
  # Start multicore
  options(future.rng.onMisuse = "ignore", future.globals.maxSize = 100 * 1024^3)  # Set to 100 GiB
  future::plan(future::multisession, workers = workers)

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
#   mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

