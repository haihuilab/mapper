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
#' mapper
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper
#' @export
mapper <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_chr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_chr
#' @export
mapper_chr <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_dbl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_dbl
#' @export
mapper_dbl <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_dfc
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_dfc
#' @export
mapper_dfc <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_dfr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_dfr
#' @export
mapper_dfr <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_int
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_int
#' @export
mapper_int <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_lgl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_lgl
#' @export
mapper_lgl <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_map_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' mapper_walk
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname mapper_walk
#' @export
mapper_walk <- function(workers = NULL, ...) {
  if (is.null(workers)) {
    workers <- parallelly::availableCores() - 2
  }
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
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
#   mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

