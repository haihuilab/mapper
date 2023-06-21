#' imapping in Parallel and Ending parallel imapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2

# library(parallel)
# library(purrr)
# library(furrr)
# library(dplyr)


# Run the removing cache function

# 1. future_imapper:future_imap------------------------
#' @rdname future_imapper
#' @export
future_imapper <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # imap function------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 2. future_imapper_chr:future_imap_chr------------------------
#' @rdname future_imapper_chr
#' @export
future_imapper_chr <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl",)


  # imap function------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 3. future_imapper_dbl:future_imap_dbl------------------------
#' @rdname future_imapper_dbl
#' @export
future_imapper_dbl <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 4. future_imapper_dfc:future_imap_dfc------------------------
#' @rdname future_imapper_dfc
#' @export
future_imapper_dfc <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 5. future_imapper_dfr:future_imap_dfr------------------------
#' @rdname future_imapper_dfr
#' @export
future_imapper_dfr <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 6. future_imapper_int:future_imap_int------------------------
#' @rdname future_imapper_int
#' @export
future_imapper_int <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 7. future_imapper_lgl:future_imap_lgl------------------------
#' @rdname future_imapper_lgl
#' @export
future_imapper_lgl <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 8. future_imapper_pwalk:future_walk------------------------
#' @rdname future_imapper_pwalk
#' @export
future_imapper_pwalk <- function(...) {
  imap_list <- c(future_imap,
                       future_imap_chr,
                       future_imap_dbl,
                       future_imap_dfc,
                       future_imap_dfr,
                       future_imap_int,
                       future_imap_lgl)

  imapper_list <- c("future_imapper",
                          "future_imapper_chr",
                          "future_imapper_dbl",
                          "future_imapper_dfc",
                          "future_imapper_dfr",
                          "future_imapper_int",
                          "future_imapper_lgl")


  # 2. future_imapper_chr:future_imap_chr------------------------
  imap_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(imap_list, function(i) imap_func(i)) %>% setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


#----------------------------------------------
# Examples
# devtools::install_github("haihuilab/imapper")
# library(imapper)
# library(tidyverse)
# Remove cache when using furrr:imap functions
# 1:10 %>%
#   future_imapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_imapper_dbl(mean)


