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
#' future_imapper
#' @rdname future_imapper
#' @export
future_imapper <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_chr
#' @rdname future_imapper_chr
#' @export
future_imapper_chr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_dbl
#' @rdname future_imapper_dbl
#' @export
future_imapper_dbl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_dfc
#' @rdname future_imapper_dfc
#' @export
future_imapper_dfc <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_dfr
#' @rdname future_imapper_dfr
#' @export
future_imapper_dfr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_int
#' @rdname future_imapper_int
#' @export
future_imapper_int <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_imap_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' future_imapper_lgl
#' @rdname future_imapper_lgl
#' @export
future_imapper_lgl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
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
#   future_imapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

