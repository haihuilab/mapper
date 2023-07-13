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
#' invoke_mapper
#' @rdname invoke_mapper
#' @export
invoke_mapper <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_chr
#' @rdname invoke_mapper_chr
#' @export
invoke_mapper_chr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_chr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_dbl
#' @rdname invoke_mapper_dbl
#' @export
invoke_mapper_dbl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_dbl(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_dfc
#' @rdname invoke_mapper_dfc
#' @export
invoke_mapper_dfc <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_dfc(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_dfr
#' @rdname invoke_mapper_dfr
#' @export
invoke_mapper_dfr <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_dfr(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_int
#' @rdname invoke_mapper_int
#' @export
invoke_mapper_int <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_int(...)
  # shut down multicore and clear cache
  future::plan(future::sequential)
  gc()
  return(res)
}


#' invoke_mapper_lgl
#' @rdname invoke_mapper_lgl
#' @export
invoke_mapper_lgl <- function(...) {
  # Start multicore
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  options(future.globals.maxSize = 5e9)
  # map function
  res <- furrr::future_invoke_map_lgl(...)
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
#   invoke_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

