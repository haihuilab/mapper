#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::invoke_map
#' @inheritParams furrr::future_invoke_map
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#' future_invoke_mapper_template
#' @export
future_invoke_mapper_template <- function(...) {
  invoke_map_list <- c(furrr::future_invoke_map,
                       furrr::future_invoke_map_chr,
                       furrr::future_invoke_map_dbl,
                       furrr::future_invoke_map_dfc,
                       furrr::future_invoke_map_dfr,
                       furrr::future_invoke_map_int,
                       furrr::future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                          "future_invoke_mapper_chr",
                          "future_invoke_mapper_dbl",
                          "future_invoke_mapper_dfc",
                          "future_invoke_mapper_dfr",
                          "future_invoke_mapper_int",
                          "future_invoke_mapper_lgl")

  # map function------------------------
  invoke_mapper_func <- function(i) {
    # Start multicore
    future::plan(future::multisession, workers = parallel::detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)

      # shut down multicore and clear cache
      future::plan(future::sequential)
      gc()

      return(res)
    }
    return(output)
  }

  func_list <- map(invoke_map_list, function(i) invoke_mapper_func(i)) %>% stats::setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


#' future_invoke_mapper
#' @rdname future_invoke_mapper
#' @export
future_invoke_mapper <- future_invoke_mapper_template()

#' future_invoke_mapper_chr
#' @rdname future_invoke_mapper_chr
#' @export
future_invoke_mapper_chr <- future_invoke_mapper_template()

#' future_invoke_mapper_dbl
#' @rdname future_invoke_mapper_dbl
#' @export
future_invoke_mapper_dbl <- future_invoke_mapper_template()

#' future_invoke_mapper_dfc
#' @rdname future_invoke_mapper_dfc
#' @export
future_invoke_mapper_dfc <- future_invoke_mapper_template()

#' future_invoke_mapper_dfr
#' @rdname future_invoke_mapper_dfr
#' @export
future_invoke_mapper_dfr <- future_invoke_mapper_template()

#' future_invoke_mapper_int
#' @rdname future_invoke_mapper_int
#' @export
future_invoke_mapper_int <- future_invoke_mapper_template()

#' future_invoke_mapper_lgl
#' @rdname future_invoke_mapper_lgl
#' @export
future_invoke_mapper_lgl <- future_invoke_mapper_template()


