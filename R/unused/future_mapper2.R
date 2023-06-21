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
future_mapper2 <- future_mapper2_template()

#' @rdname future_mapper2_chr
#' @export
future_mapper2_chr <- future_mapper2_template()

#' @rdname future_mapper2_dbl
#' @export
future_mapper2_dbl <- future_mapper2_template()

#' @rdname future_mapper2_dfc
#' @export
future_mapper2_dfc <- future_mapper2_template()

#' @rdname future_mapper2_dfr
#' @export
future_mapper2_dfr <- future_mapper2_template()

#' @rdname future_mapper2_int
#' @export
future_mapper2_int <- future_mapper2_template()

#' @rdname future_mapper2_lgl
#' @export
future_mapper2_lgl <- future_mapper2_template()

#' @rdname future_mapper2_walk2
#' @export
future_mapper2_walk2 <- future_mapper2_template()


