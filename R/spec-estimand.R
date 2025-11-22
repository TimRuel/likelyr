# ======================================================================
# Estimand Specification
# ======================================================================

#' Estimand specification for likelihood workflows
#'
#' @description
#' Defines ψ(θ), its optional gradient, and confidence-level structure
#' used for integrated/profile likelihood computations. The user must
#' supply a fixed search interval for ψ, which governs branch-mode
#' optimization.
#'
#' @param psi_fn Function(theta, data) → numeric scalar.
#' @param psi_jac Optional function(theta, data) → gradient vector of psi.
#'
#' @param search_interval Numeric length-2 vector c(lower, upper)
#'        specifying the allowable search range for branch modes.
#'
#' @param increment Positive numeric scalar specifying the distance
#'        between consecutive psi values at which the pseudolikelihood
#'        is evaluated
#'
#' @param confidence_levels Numeric vector in (0,1). Example:
#'        c(0.90, 0.95, 0.99). Used for plotting and to derive
#'        `alpha_target = min(1 - confidence_levels)`.
#'
#' @param name Optional descriptive label.
#' @param ... Extra metadata.
#'
#' @return An S3 object of class `likelihood_estimand`.
#' @export
estimand_spec <- function(
    psi_fn,
    search_interval,
    increment,
    confidence_levels = c(0.90, 0.95, 0.99),
    psi_jac = NULL,
    name = NULL,
    ...
) {

  # ------------------------------------------------------------
  # Validate psi_fn
  # ------------------------------------------------------------
  if (!is.function(psi_fn))
    stop("`psi_fn` must be a function(theta, data).", call. = FALSE)

  # ------------------------------------------------------------
  # Validate search interval
  # ------------------------------------------------------------
  if (!is.numeric(search_interval) ||
      length(search_interval) != 2 ||
      any(!is.finite(search_interval))) {
    stop("`search_interval` must be a numeric, finite vector of length 2.",
         call. = FALSE)
  }
  if (search_interval[1] >= search_interval[2])
    stop("`search_interval[1]` must be strictly less than `search_interval[2]`.",
         call. = FALSE)

  # ------------------------------------------------------------
  # Validate increment
  # ------------------------------------------------------------
  if (!is.numeric(increment) ||
      length(increment) != 1 ||
      increment <= 0) {
    stop("`increment` must be a positive numeric scalar.",
         call. = FALSE)
  }

  # ------------------------------------------------------------
  # psi_jac validation
  # ------------------------------------------------------------
  if (!is.null(psi_jac) && !is.function(psi_jac))
    stop("`psi_jac` must be a gradient function(theta, data) when supplied.",
         call. = FALSE)

  # ------------------------------------------------------------
  # Confidence level validation
  # ------------------------------------------------------------
  if (!is.numeric(confidence_levels) ||
      any(confidence_levels <= 0 | confidence_levels >= 1)) {
    stop("`confidence_levels` must be numbers strictly between 0 and 1.",
         call. = FALSE)
  }

  # --------------------------------------------------------------------
  # alpha_target drives how deep each branch must run
  # --------------------------------------------------------------------
  alpha_target <- min(1 - confidence_levels)

  x <- list(
    name              = name %||% "<estimand>",
    psi_fn            = psi_fn,
    psi_jac           = psi_jac,
    search_interval   = search_interval,
    increment         = increment,
    confidence_levels = confidence_levels,
    alpha_target      = alpha_target,
    extra             = list(...)
  )

  class(x) <- "likelihood_estimand"
  x
}

# ======================================================================
# Print
# ======================================================================

#' @export
print.likelihood_estimand <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Confidence Levels:   ",
      paste0(100 * x$confidence_levels, "%", collapse = ", "),
      "\n", sep = "")
  cat("- alpha_target:        ", x$alpha_target, "\n", sep = "")
  cat("- search_interval:     ",
      paste(x$search_interval, collapse = "  "), "\n", sep = "")
  cat("- increment:           ", x$increment, "\n", sep = "")
  cat("- Has psi_jac:         ", !is.null(x$psi_jac), "\n", sep = "")
  invisible(x)
}
