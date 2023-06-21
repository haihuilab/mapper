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
#'
#' @export


#' @rdname future_mapper2
#' @export
future_mapper2 <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_chr
#' @export
future_mapper2_chr <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_dbl
#' @export
future_mapper2_dbl <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_dfc
#' @export
future_mapper2_dfc <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_dfr
#' @export
future_mapper2_dfr <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_int
#' @export
future_mapper2_int <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_lgl
#' @export
future_mapper2_lgl <- function(...) {
  future_mapper2(...)
}


#' @rdname future_mapper2_walk2
#' @export
future_mapper2_walk2 <- function(...) {
  future_mapper2(...)
}
