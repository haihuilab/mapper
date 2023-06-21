#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::map
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#' future_mapper_template
#' @export
future_mapper_template <- function(...) {
  map_list <- c(furrr::future_map,
                furrr::future_map_chr,
                furrr::future_map_dbl,
                furrr::future_map_dfc,
                furrr::future_map_dfr,
                furrr::future_map_int,
                furrr::future_map_lgl,
                furrr::future_walk)

  mapper_list <- c("future_mapper",
                   "future_mapper_chr",
                   "future_mapper_dbl",
                   "future_mapper_dfc",
                   "future_mapper_dfr",
                   "future_mapper_int",
                   "future_mapper_lgl",
                   "future_mapper_walk")

  # map function------------------------
  map_func <- function(i) {
    # Start multicore
    future::plan(future::multisession, workers = parallel::detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    future::plan(future::sequential)
    gc()

    return(output)
  }

  func_list <- map(map_list, function(i) map_func(i)) %>% stats::setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}
