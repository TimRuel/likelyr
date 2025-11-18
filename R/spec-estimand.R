# ======================================================================
# Estimand Specification
# ======================================================================

#' Estimand specification for likelihood workflows
#'
#' @description
#' Defines ψ(θ) and optional analytic derivative dψ/dθ, along with
#' confidence levels used for plotting confidence bands of the
#' integrated and profile likelihood curves.
#'
#' @param psi Function(theta, ...) → numeric scalar representing ψ(θ).
#' @param dpsi Optional gradient function(theta, ...) → numeric vector.
#' @param confidence_levels Numeric vector of desired confidence levels
#'   (values must be strictly between 0 and 1). Default: c(0.90, 0.95).
#' @param name Optional label for printing.
#' @param ... Extra user-defined fields stored as metadata.
#'
#' @return S3 object of class `likelihood_estimand`.
#' @export
estimand_spec <- function(psi,
                          dpsi = NULL,
                          confidence_levels = c(0.90, 0.95, 0.99),
                          name = NULL,
                          ...) {

  # Compute alpha_target from confidence levels
  if (length(confidence_levels) == 0)
    stop("`confidence_levels` must contain at least one value.", call. = FALSE)

  alpha_vec <- 1 - confidence_levels
  alpha_target <- min(alpha_vec)

  x <- list(
    name              = name %||% "<estimand>",
    psi               = psi,
    dpsi              = dpsi,
    confidence_levels = confidence_levels,
    alpha_levels      = alpha_vec,
    alpha_target      = alpha_target,
    extra             = list(...)
  )
  class(x) <- "likelihood_estimand"
  validate_estimand_spec(x)
  x
}

# ----------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------

validate_estimand_spec <- function(x) {

  if (!is.function(x$psi))
    stop("`psi` must be a function.", call. = FALSE)

  if (!is.null(x$dpsi) && !is.function(x$dpsi))
    stop("`dpsi` must be a function when supplied.", call. = FALSE)

  if (!is.numeric(x$confidence_levels) ||
      any(x$confidence_levels <= 0 | x$confidence_levels >= 1))
    stop("`confidence_levels` must contain numeric values strictly between 0 and 1.",
         call. = FALSE)

  invisible(x)
}

# ----------------------------------------------------------------------
# Printing
# ----------------------------------------------------------------------

#' @export
print.likelihood_estimand <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:              ", x$name, "\n", sep = "")
  cat("- Has dpsi():        ", !is.null(x$dpsi), "\n", sep = "")
  cat("- Confidence Levels: ", paste0(x$confidence_levels, collapse = ", "), "\n", sep = "")
  cat("- alpha_target:      ", x$alpha_target, "\n", sep = "")
  invisible(x)
}
