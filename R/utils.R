#' Null-coalescing operator
#'
#' @description
#' Returns \code{y} if \code{x} is \code{NULL}, otherwise returns \code{x}.
#' This is a small convenience helper used throughout the codebase
#' to provide default values.
#'
#' @param x Primary value.
#' @param y Fallback value used when \code{x} is \code{NULL}.
#'
#' @return Either \code{x} or \code{y}.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
