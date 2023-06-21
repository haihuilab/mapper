#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::pmap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::pmap
#' @inheritParams furrr::future_pmap
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#' future_pmapper_template
#' @export
future_pmapper_template <- function(...) {
  pmap_list <- c(furrr::future_pmap,
                 furrr::future_pmap_chr,
                 furrr::future_pmap_dbl,
                 furrr::future_pmap_dfc,
                 furrr::future_pmap_dfr,
                 furrr::future_pmap_int,
                 furrr::future_pmap_lgl,
                 furrr::future_pwalk)

  pmapper_list <- c("future_pmapper",
                    "future_pmapper_chr",
                    "future_pmapper_dbl",
                    "future_pmapper_dfc",
                    "future_pmapper_dfr",
                    "future_pmapper_int",
                    "future_pmapper_lgl",
                    "future_pmapper_pwalk")

  # map function------------------------
  pmapper_func <- function(i) {
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

  func_list <- map(pmap_list, function(i) pmapper_func(i)) %>% stats::setNames(pmapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}



#' @rdname future_pmapper
#' @export
future_pmapper <- future_pmapper_template()

#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- future_pmapper_template()

#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- future_pmapper_template()

#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- future_pmapper_template()

#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- future_pmapper_template()

#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- future_pmapper_template()

#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- future_pmapper_template()

#' @rdname future_pmapper_pwalk
#' @export
future_pmapper_pwalk <- future_pmapper_template()

