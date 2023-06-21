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
future_mapper <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_chr
#' @export
future_mapper_chr <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_dbl
#' @export
future_mapper_dbl <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_dfc
#' @export
future_mapper_dfc <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_dfr
#' @export
future_mapper_dfr <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_int
#' @export
future_mapper_int <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_lgl
#' @export
future_mapper_lgl <- function(...) {
  future_mapper(...)
}


#' @rdname future_mapper_walk
#' @export
future_mapper_walk <- function(...) {
  future_mapper(...)
}
