#' @keywords internal
assert_function <- function(x, name, length = NULL) {
  if (!is.function(x)) {
    stop(sprintf("`%s` must be a function.", name), call. = FALSE)
  }
  if (!is.null(length) && length(formals(x)) != length) {
    warning(sprintf("`%s` does not have the expected number of formals.", name), call. = FALSE)
  }
}

#' @keywords internal
assert_scalar_integer <- function(x, name, min = NULL, max = NULL) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x %% 1 != 0) {
    stop(sprintf("`%s` must be a scalar integer.", name), call. = FALSE)
  }
  if (!is.null(min) && x < min) {
    stop(sprintf("`%s` must be >= %d.", name, min), call. = FALSE)
  }
  if (!is.null(max) && x > max) {
    stop(sprintf("`%s` must be <= %d.", name, max), call. = FALSE)
  }
}

#' @keywords internal
assert_character_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L) {
    stop(sprintf("`%s` must be a length-1 character vector.", name), call. = FALSE)
  }
}

#' @keywords internal
assert_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
}
