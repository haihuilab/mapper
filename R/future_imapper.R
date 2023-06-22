#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::imap
#' @inheritParams furrr::imap
#' @importFrom magrittr `%>%`
#' @importFrom purrr map


#' @return
#' A vector the same length as .x.
#'

#' future_imapper_template
#' @export
future_imapper_template <- function(...) {
  imap_list <- c(furrr::future_imap,
                 furrr::future_imap_chr,
                 furrr::future_imap_dbl,
                 furrr::future_imap_dfc,
                 furrr::future_imap_dfr,
                 furrr::future_imap_int,
                 furrr::future_imap_lgl)

  imapper_list <- c("future_imapper",
                    "future_imapper_chr",
                    "future_imapper_dbl",
                    "future_imapper_dfc",
                    "future_imapper_dfr",
                    "future_imapper_int",
                    "future_imapper_lgl")

  # imap function------------------------
  imap_func <- function(i) {
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

  func_list <- map(imap_list, function(i) imap_func(i)) %>% stats::setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}

#' future_imapper
#' @rdname future_imapper
#' @export
future_imapper <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper(...)
  }
}


#' future_imapper_chr
#' @rdname future_imapper_chr
#' @export
future_imapper_chr <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_chr(...)
  }
}

#' future_imapper_dbl
#' @rdname future_imapper_dbl
#' @export
future_imapper_dbl <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_dbl(...)
  }
}

#' future_imapper_dfc
#' @rdname future_imapper_dfc
#' @export
future_imapper_dfc <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_dfc(...)
  }
}

#' future_imapper_dfr
#' @rdname future_imapper_dfr
#' @export
future_imapper_dfr <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_dfr(...)
  }
}

#' future_imapper_int
#' @rdname future_imapper_int
#' @export
future_imapper_int <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_int(...)
  }
}

#' future_imapper_lgl
#' @rdname future_imapper_lgl
#' @export
future_imapper_lgl <- function(...) {
  flag <- FALSE
  if (!flag) {
    future_imapper_template()
    flag <- TRUE
  } else {
    future_imapper_lgl(...)
  }
}

