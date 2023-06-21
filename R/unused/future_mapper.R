#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::map
#' @inheritParams furrr::map
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#'
#' @export

#' @rdname future_mapper
#' @export
future_mapper <- future_mapper_template()

#' @rdname future_mapper_chr
#' @export
future_mapper_chr <- future_mapper_template()

#' @rdname future_mapper_dbl
#' @export
future_mapper_dbl <- future_mapper_template()

#' @rdname future_mapper_dfc
#' @export
future_mapper_dfc <- future_mapper_template()

#' @rdname future_mapper_dfr
#' @export
future_mapper_dfr <- future_mapper_template()

#' @rdname future_mapper_int
#' @export
future_mapper_int <- future_mapper_template()

#' @rdname future_mapper_lgl
#' @export
future_mapper_lgl <- future_mapper_template()


#' @rdname future_mapper_walk
#' @export
future_mapper_walk <- future_mapper_template()

# Example
# library(tidyverse)
# library(furrr)
# # Remove cache when using furrr:map functions
# 1:10 %>%
#   future_mapper(rnorm, n = 10, .options = furrr_options(seed = 1233)) %>%
#   future_map_dbl(mean)

