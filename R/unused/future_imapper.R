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
future_imapper <- future_imapper_template()

#' @rdname future_imapper_chr
#' @export
future_imapper_chr <- future_imapper_template()

#' @rdname future_imapper_dbl
#' @export
future_imapper_dbl <- future_imapper_template()

#' @rdname future_imapper_dfc
#' @export
future_imapper_dfc <- future_imapper_template()

#' @rdname future_imapper_dfr
#' @export
future_imapper_dfr <- future_imapper_template()

#' @rdname future_imapper_int
#' @export
future_imapper_int <- future_imapper_template()

#' @rdname future_imapper_lgl
#' @export
future_imapper_lgl <- future_imapper_template()

