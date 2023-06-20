#' MAPping in Parallel and Ending parallel mapping `furrr` in R
#'
#' @param workers default parallel workers are detectCores()-2

# library(parallel)
# library(purrr)
# library(furrr)
# library(dplyr)


# Run the removing cache function

# 1. future_mapper2:future_pmap------------------------
#' @rdname future_mapper2
#' @export
future_mapper2 <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 2. future_mapper2_chr:future_pmap_chr------------------------
#' @rdname future_mapper2_chr
#' @export
future_mapper2_chr <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 3. future_mapper2_dbl:future_pmap_dbl------------------------
#' @rdname future_mapper2_dbl
#' @export
future_mapper2_dbl <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 4. future_mapper2_dfc:future_pmap_dfc------------------------
#' @rdname future_mapper2_dfc
#' @export
future_mapper2_dfc <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 5. future_mapper2_dfr:future_pmap_dfr------------------------
#' @rdname future_mapper2_dfr
#' @export
future_mapper2_dfr <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 6. future_mapper2_int:future_pmap_int------------------------
#' @rdname future_mapper2_int
#' @export
future_mapper2_int <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 7. future_mapper2_lgl:future_pmap_lgl------------------------
#' @rdname future_mapper2_lgl
#' @export
future_mapper2_lgl <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


# 8. future_mapper2_walk2:future_walk------------------------
#' @rdname future_mapper2_walk2
#' @export
future_mapper2_walk2 <- function(...) {
  map_list <- c(future_map2,
                future_map2_chr,
                future_map2_dbl,
                future_map2_dfc,
                future_map2_dfr,
                future_map2_int,
                future_map2_lgl,
                future_walk2)

  mapper_list <- c("future_mapper2",
                   "future_mapper2_chr",
                   "future_mapper2_dbl",
                   "future_mapper2_dfc",
                   "future_mapper2_dfr",
                   "future_mapper2_int",
                   "future_mapper2_lgl",
                   "future_mapper2_walk2")


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


