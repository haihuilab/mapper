#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::imap
#' @inheritParams furrr::imap
#' @importFrom magrittr `%>%`
#' @importFrom purrr imap
#'
#' @return parameters
#' @export
#'
#' imapper
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper
#' @export
imapper <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_chr
#' @param workers default value is the `total cores - 2`
#' @rdname imapper_chr
#' @export
imapper_chr <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_dbl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper_dbl
#' @export
imapper_dbl <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_dfc
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper_dfc
#' @export
imapper_dfc <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_dfr
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper_dfr
#' @export
imapper_dfr <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_int
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper_int
#' @export
imapper_int <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' imapper_lgl
#' @param workers multicore numbers: default value is the `total cores - 2`
#' @rdname imapper_lgl
#' @export
imapper_lgl <- function(workers = parallel::detectCores()-2, ...) {
  # Start multicore
  future::plan(future::multisession, workers = workers)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_imap_lgl(...)
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
#   imapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

