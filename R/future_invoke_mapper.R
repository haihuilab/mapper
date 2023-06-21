#' invoke_mapping in Parallel and Ending parallel invoke_mapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2

# library(parallel)
# library(purrr)
# library(furrr)
# library(dplyr)


# Run the removing cache function

# 1. future_invoke_mapper:future_invoke_map------------------------
#' @rdname future_invoke_mapper
#' @export
future_invoke_mapper <- function(...) {
  invoke_map_list <- c(future_invoke_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # invoke_map function------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
#' @rdname future_invoke_mapper_chr
#' @export
future_invoke_mapper_chr <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # invoke_map function------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 3. future_invoke_mapper_dbl:future_invoke_map_dbl------------------------
#' @rdname future_invoke_mapper_dbl
#' @export
future_invoke_mapper_dbl <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 4. future_invoke_mapper_dfc:future_invoke_map_dfc------------------------
#' @rdname future_invoke_mapper_dfc
#' @export
future_invoke_mapper_dfc <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 5. future_invoke_mapper_dfr:future_invoke_map_dfr------------------------
#' @rdname future_invoke_mapper_dfr
#' @export
future_invoke_mapper_dfr <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 6. future_invoke_mapper_int:future_invoke_map_int------------------------
#' @rdname future_invoke_mapper_int
#' @export
future_invoke_mapper_int <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 7. future_invoke_mapper_lgl:future_invoke_map_lgl------------------------
#' @rdname future_invoke_mapper_lgl
#' @export
future_invoke_mapper_lgl <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


# 8. future_invoke_mapper_pwalk:future_walk------------------------
#' @rdname future_invoke_mapper_pwalk
#' @export
future_invoke_mapper_pwalk <- function(...) {
  invoke_map_list <- c(future_invoke_map,
                future_invoke_map_chr,
                future_invoke_map_dbl,
                future_invoke_map_dfc,
                future_invoke_map_dfr,
                future_invoke_map_int,
                future_invoke_map_lgl)

  invoke_mapper_list <- c("future_invoke_mapper",
                   "future_invoke_mapper_chr",
                   "future_invoke_mapper_dbl",
                   "future_invoke_mapper_dfc",
                   "future_invoke_mapper_dfr",
                   "future_invoke_mapper_int",
                   "future_invoke_mapper_lgl")


  # 2. future_invoke_mapper_chr:future_invoke_map_chr------------------------
  invoke_map_func <- function(i) {
    # Start multicore
    plan(multisession, workers = detectCores()-2)
    options(future.globals.maxSize = 5000000000)

    inner_func <- i
    output <-  function(...) {
      # invoke_map function
      res <- inner_func(...)
      return(res)

    }
    # shut down multicore and clear cache
    plan(sequential)
    gc()

    return(output)
  }


  func_list <- map(invoke_map_list, function(i) invoke_map_func(i)) %>% setNames(invoke_mapper_list)
  # Extract the functions as individual ones
  list2env(func_list, envir = .GlobalEnv)
}


#----------------------------------------------
# Examples
# devtools::install_github("haihuilab/invoke_mapper")
# library(invoke_mapper)
# library(tidyverse)
# Remove cache when using furrr:invoke_map functions
# 1:10 %>%
#   future_invoke_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_invoke_mapper_dbl(mean)


