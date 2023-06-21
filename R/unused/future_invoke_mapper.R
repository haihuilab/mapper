#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::invoke_map
#' @inheritParams furrr::future_invoke_map
#' @importFrom magrittr `%>%`
#' @importFrom purrr map

#'
#' @return
#' A vector the same length as .x.
#'
#' @export

#' @rdname future_invoke_mapper
#' @export
future_invoke_mapper <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_chr
#' @export
future_invoke_mapper_chr <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_dbl
#' @export
future_invoke_mapper_dbl <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_dfc
#' @export
future_invoke_mapper_dfc <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_dfr
#' @export
future_invoke_mapper_dfr <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_int
#' @export
future_invoke_mapper_int <- future_invoke_mapper_template()

#' @rdname future_invoke_mapper_lgl
#' @export
future_invoke_mapper_lgl <- future_invoke_mapper_template()


