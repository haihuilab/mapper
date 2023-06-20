#' MAPping in Parallel and Ending parallel mapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2

# library(parallel)
# library(purrr)
# library(furrr)
# library(dplyr)


# Run the removing cache function

# 1. future_pmapper:future_pmap------------------------
#' @rdname future_pmapper
#' @export
future_pmapper <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_walk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # map function------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 2. future_pmapper_chr:future_pmap_chr------------------------
#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # map function------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 3. future_pmapper_dbl:future_pmap_dbl------------------------
#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 4. future_pmapper_dfc:future_pmap_dfc------------------------
#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 5. future_pmapper_dfr:future_pmap_dfr------------------------
#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 6. future_pmapper_int:future_pmap_int------------------------
#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 7. future_pmapper_lgl:future_pmap_lgl------------------------
#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_iwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 8. future_pmapper_pwalk:future_walk------------------------
#' @rdname future_pmapper_pwalk
#' @export
future_pmapper_pwalk <- function(...) {
  map_list <- c(future_pmap,
                future_pmap_chr,
                future_pmap_dbl,
                future_pmap_dfc,
                future_pmap_dfr,
                future_pmap_int,
                future_pmap_lgl,
                future_pwalk)

  mapper_list <- c("future_pmapper",
                   "future_pmapper_chr",
                   "future_pmapper_dbl",
                   "future_pmapper_dfc",
                   "future_pmapper_dfr",
                   "future_pmapper_int",
                   "future_pmapper_lgl",
                   "future_pmapper_pwalk")


  # 2. future_mapper_chr:future_map_chr------------------------
  map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(map_list, function(i) map_func(i)) %>% setNames(mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


#----------------------------------------------
# Examples
# devtools::install_github("haihuilab/mapper")
# library(mapper)
# library(tidyverse)
# Remove cache when using furrr:map functions
# 1:10 %>%
#   future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_mapper_dbl(mean)


