#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::invoke_map
#' @inheritParams furrr::invoke_map
#' @importFrom magrittr `%>%`
#' @importFrom purrr invoke_map
#'
#' @return parameters
#' @export
#'
#' future_invoke_mapper
#' @rdname future_invoke_mapper
#' @export
future_invoke_mapper <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_chr
#' @rdname future_invoke_mapper_chr
#' @export
future_invoke_mapper_chr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_dbl
#' @rdname future_invoke_mapper_dbl
#' @export
future_invoke_mapper_dbl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_dfc
#' @rdname future_invoke_mapper_dfc
#' @export
future_invoke_mapper_dfc <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_dfr
#' @rdname future_invoke_mapper_dfr
#' @export
future_invoke_mapper_dfr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_int
#' @rdname future_invoke_mapper_int
#' @export
future_invoke_mapper_int <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}


#' future_invoke_mapper_lgl
#' @rdname future_invoke_mapper_lgl
#' @export
future_invoke_mapper_lgl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5000000000)
  # map function
  res <- furrr::future_invoke_map_lgl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)

  return(res)
}



# Example
# library(tidyverse)
# library(furrr)
# # Remove cache when using furrr:map functions
# 1:10 %>%
#   future_invoke_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

