
#' future_mapper
#' @param ... Input parameter
#' @return Output result
#' @name future_mapper
#' @export
future_mapper1 <- function(...) {
  future_mapper_template()
  future_mapper(...)
}

# Example
# library(mapper)
# library(tidyverse)
# 1:10 %>%
#   future_mapper(rnorm, n = 10)

