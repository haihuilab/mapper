#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::map2()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map2
#' @inheritParams furrr::future_map2
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#' future_mapper2_template
#' @export
future_mapper2_template <- function(arg) {
  map2_list <- c(furrr::future_map2,
                 furrr::future_map2_chr,
                 furrr::future_map2_dbl,
                 furrr::future_map2_dfc,
                 furrr::future_map2_dfr,
                 furrr::future_map2_int,
                 furrr::future_map2_lgl,
                 furrr::future_walk2)

  mapper2_list <- c("future_mapper2",
                    "future_mapper2_chr",
                    "future_mapper2_dbl",
                    "future_mapper2_dfc",
                    "future_mapper2_dfr",
                    "future_mapper2_int",
                    "future_mapper2_lgl",
                    "future_mapper2_walk2")

  # map function------------------------
  mapper2_func <- function(i) {
    inner_func <- i
    output <-  function(...) {
      # Start multicore
      future::plan(future::multisession, workers = parallel::detectCores()-2)
      options(future.globals.maxSize = 5000000000)

      # map function
      res <- inner_func(...)

      # shut down multicore and clear cache
      future::plan(future::sequential)
      gc()

      return(res)
    }
    return(output)
  }

  func_list <- map(map2_list[which(mapper2_list == arg)], function(i) mapper2_func(i)) %>% stats::setNames(arg)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


#' @rdname future_mapper2
#' @export
future_mapper2 <- function(...) {
    future_mapper2_template("future_mapper2")
    future_mapper2(...)
}

#' @rdname future_mapper2_chr
#' @export
future_mapper2_chr <- function(...) {
    future_mapper2_template("future_mapper2_chr")
    future_mapper2_chr(...)
}

#' @rdname future_mapper2_dbl
#' @export
future_mapper2_dbl <- function(...) {
    future_mapper2_template("future_mapper2_dbl")
    future_mapper2_dbl(...)
}

#' @rdname future_mapper2_dfc
#' @export
future_mapper2_dfc <- function(...) {
    future_mapper2_template("future_mapper2_dfc")
    future_mapper2_dfc(...)
}

#' @rdname future_mapper2_dfr
#' @export
future_mapper2_dfr <- function(...) {
    future_mapper2_template("future_mapper2_dfr")
    future_mapper2_dfr(...)
}

#' @rdname future_mapper2_int
#' @export
future_mapper2_int <- function(...) {
    future_mapper2_template("future_mapper2_int")
    future_mapper2_int(...)
}

#' @rdname future_mapper2_lgl
#' @export
future_mapper2_lgl <- function(...) {
    future_mapper2_template("future_mapper2_lgl")
    future_mapper2_lgl(...)
}

#' @rdname future_mapper2_walk2
#' @export
future_mapper2_walk2 <- function(...) {
    future_mapper2_template("future_mapper2_walk2")
    future_mapper2_walk2(...)
}


