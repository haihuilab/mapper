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
#'
#' @export


#' @rdname future_pmapper
#' @export
future_pmapper <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- function(...) {
  future_pmapper(...)
}


#' @rdname future_pmapper_pwalk
#' @export
future_pmapper_pwalk <- function(...) {
  future_pmapper(...)
}
