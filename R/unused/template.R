#' mapper
#'
#' @param ... parameter setting is the same as furrr package
#'
#' @export
#'
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
    # Start multicore
    future::plan(future::multisession, workers = parallel::detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # imap function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    future::plan(future::sequential)
    gc()

    return(output)
  }

  func_list <- map(imap_list, function(i) imap_func(i)) %>% stats::setNames(imapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 2. future_mapper-----------------------------------
#' future_mapper_template
#' @param ... parameters as future map functions
#' @rdname future_mapper_template
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


# 3. future_mapper2----------------------------------
#' future_mapper2_template
#' @param ... parameters as future map functions
#' @rdname future_mapper2
#' @export
future_mapper2_template <- function(...) {
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

  func_list <- map(map2_list, function(i) mapper2_func(i)) %>% stats::setNames(mapper2_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 4. future_pmapper----------------------------------
#' future_pmapper_template
#' @param ... parameters as future map functions
#' @rdname future_pmapper_template
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


# 5. future_invoke_mapper----------------------------------
#' future_invoke_mapper_template
#' @param ... parameters as future map functions
#' @rdname future_invoke_mapper_template
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
      return(res)

    }
    # shut down multicore and clear cache
    future::plan(future::sequential)
    gc()

    return(output)
  }

  func_list <- map(invoke_map_list, function(i) invoke_mapper_func(i)) %>% stats::setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


