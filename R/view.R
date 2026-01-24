# ======================================================================
# view.R
# S3 generic for semantic object viewers
# ======================================================================

#' View an object
#'
#' @description
#' Render the canonical human-facing representation of an object.
#'
#' Unlike `View()`, this is a semantic viewer that may print tables,
#' plots, or structured summaries.
#'
#' @param x Object to view.
#' @param ... Passed to methods.
#'
#' @export
view <- function(x, ...) {
  .assert_local_rendering()
  UseMethod("view")
}

#' @export
view.default <- function(x, ...) {
  stop(
    sprintf(
      "No view() method for objects of class '%s'.",
      paste(class(x), collapse = "/")
    ),
    call. = FALSE
  )
}
