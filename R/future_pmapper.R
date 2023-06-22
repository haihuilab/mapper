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
future_pmapper_template <- function(arg) {
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
  func_list <- map(pmap_list[which(pmapper_list == arg)], function(i) pmapper_func(i)) %>% stats::setNames(arg)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}



#' @rdname future_pmapper
#' @export
future_pmapper <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper")
    flag <- TRUE
  } else {
    future_pmapper(...)
  }
}

#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_chr")
    flag <- TRUE
  } else {
    future_pmapper_chr(...)
  }
}

#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_dbl")
    flag <- TRUE
  } else {
    future_pmapper_dbl(...)
  }
}

#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_dfc")
    flag <- TRUE
  } else {
    future_pmapper_dfc(...)
  }
}

#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_dfr")
    flag <- TRUE
  } else {
    future_pmapper_dfr(...)
  }
}

#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_int")
    flag <- TRUE
  } else {
    future_pmapper_int(...)
  }
}

#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_lgl")
    flag <- TRUE
  } else {
    future_pmapper_lgl(...)
  }
}

#' @rdname future_pmapper_pwalk
#' @export
future_pmapper_pwalk <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_pmapper_template("future_pmapper_pwalk")
    flag <- TRUE
  } else {
    future_pmapper+pwalk(...)
  }
}

