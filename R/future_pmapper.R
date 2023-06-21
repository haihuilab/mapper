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
future_pmapper <- future_pmapper_template()

#' @rdname future_pmapper_chr
#' @export
future_pmapper_chr <- future_pmapper_template()

#' @rdname future_pmapper_dbl
#' @export
future_pmapper_dbl <- future_pmapper_template()

#' @rdname future_pmapper_dfc
#' @export
future_pmapper_dfc <- future_pmapper_template()

#' @rdname future_pmapper_dfr
#' @export
future_pmapper_dfr <- future_pmapper_template()

#' @rdname future_pmapper_int
#' @export
future_pmapper_int <- future_pmapper_template()

#' @rdname future_pmapper_lgl
#' @export
future_pmapper_lgl <- future_pmapper_template()

#' @rdname future_pmapper_pwalk
#' @export
future_pmapper_pwalk <- future_pmapper_template()

