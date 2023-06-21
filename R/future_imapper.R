#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::imap
#' @inheritParams furrr::imap
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#' @return
#' A vector the same length as .x.
#'
#' @export

#' @rdname future_imapper
#' @export
future_imapper <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_chr
#' @export
future_imapper_chr <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_dbl
#' @export
future_imapper_dbl <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_dfc
#' @export
future_imapper_dfc <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_dfr
#' @export
future_imapper_dfr <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_int
#' @export
future_imapper_int <- function(...) {
  future_imapper(...)
}


#' @rdname future_imapper_lgl
#' @export
future_imapper_lgl <- function(...) {
  future_imapper(...)
}

