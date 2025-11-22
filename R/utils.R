`%||%` <- function(x, y) if (is.null(x)) y else x

dropNulls <- function(x) x[!vapply(x, is.null, logical(1))]
