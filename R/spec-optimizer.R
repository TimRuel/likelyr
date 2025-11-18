#' Optimizer specification
#'
#' @description Encapsulates the numerical optimizer backend and configuration.
#'
#' @param method Character: "nloptr", "optim", "none".
#' @param control List of optimizer control parameters.
#' @param requires_gradient Logical; TRUE if gradient must be provided.
#' @param name Optional label.
#' @param ... Extra fields.
#'
#' @return S3 object of class `likelihood_optimizer`.
#' @export
optimizer_spec <- function(method = c("nloptr", "optim", "none"),
                           control = list(),
                           requires_gradient = FALSE,
                           name = NULL,
                           ...) {

  method <- match.arg(method)

  x <- list(
    name              = name %||% "<optimizer>",
    method            = method,
    control           = control,
    requires_gradient = requires_gradient,
    extra             = list(...)
  )
  class(x) <- "likelihood_optimizer"
  validate_optimizer_spec(x)
  x
}

validate_optimizer_spec <- function(x) {
  if (!is.logical(x$requires_gradient) || length(x$requires_gradient) != 1L)
    stop("`requires_gradient` must be TRUE or FALSE.", call. = FALSE)

  invisible(x)
}

#' @export
print.likelihood_optimizer <- function(x, ...) {
  cat("# Optimizer Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Method:              ", x$method, "\n", sep = "")
  cat("- Requires gradient:   ", x$requires_gradient, "\n", sep = "")
  invisible(x)
}
