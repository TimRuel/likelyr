# ======================================================================
# Estimand Specification
# ======================================================================

#' Specify an Estimand for Likelihood Workflows
#'
#' @description
#' Defines the estimand \eqn{\psi(\theta)} and its optional gradient,
#' together with the search grid and confidence-level structure used by
#' both profile likelihood and Monte Carlo integrated likelihood.
#'
#' The estimand specification determines:
#'
#' * the scalar mapping \eqn{\theta \mapsto \psi(\theta)},
#' * the search interval in which branch modes are located,
#' * the grid spacing used for branch sweeps,
#' * the set of confidence levels used in summaries and plots,
#' * the corresponding global tail probability
#'   \eqn{\alpha_\mathrm{target} = \min(1 - \text{confidence\_levels})}.
#'
#' @param psi_fn Function(theta, data) → scalar ψ(θ)
#' @param search_interval Length-2 numeric vector c(lower, upper)
#' @param increment Positive scalar giving ψ-grid spacing
#' @param confidence_levels Numeric vector ⊂ (0,1)
#' @param psi_jac Optional gradient(theta, data)
#' @param name Optional label
#' @param ... Additional metadata
#'
#' @return An S3 object of class `"likelihood_estimand"`
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
  # Construct list
  x <- list(
    name              = name %||% "<estimand>",
    psi_fn            = psi_fn,
    psi_jac           = psi_jac,
    search_interval   = search_interval,
    increment         = increment,
    confidence_levels = confidence_levels,
    alpha_target      = min(1 - confidence_levels),
    extra             = list(...)
  )

  # Assign class then validate
  class(x) <- "likelihood_estimand"
  .validate_estimand_spec(x)
  x
}


# ======================================================================
# INTERNAL VALIDATOR (not exported)
# ======================================================================

#' @keywords internal
.validate_estimand_spec <- function(x) {

  # ψ function
  if (!is.function(x$psi_fn))
    stop("`psi_fn` must be a function(theta, data).", call. = FALSE)

  # Search interval
  si <- x$search_interval
  if (!is.numeric(si) || length(si) != 2 || any(!is.finite(si)))
    stop("`search_interval` must be a finite numeric vector of length 2.",
         call. = FALSE)
  if (si[1] >= si[2])
    stop("`search_interval` must satisfy lower < upper.", call. = FALSE)

  # Increment
  if (!is.numeric(x$increment) ||
      length(x$increment) != 1 ||
      x$increment <= 0)
    stop("`increment` must be a strictly positive scalar.", call. = FALSE)

  # psi_jac
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac))
    stop("`psi_jac` must be a function(theta, data) if supplied.",
         call. = FALSE)

  # Confidence levels
  cl <- x$confidence_levels
  if (!is.numeric(cl) ||
      any(cl <= 0 | cl >= 1))
    stop("`confidence_levels` must be strictly inside (0,1).",
         call. = FALSE)

  invisible(x)
}


# ======================================================================
# Print method
# ======================================================================

#' @export
print.likelihood_estimand <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Confidence levels:   ",
      paste0(100 * x$confidence_levels, "%", collapse = ", "),
      "\n", sep = "")
  cat("- alpha_target:        ", x$alpha_target, "\n", sep = "")
  cat("- search_interval:     ",
      paste(x$search_interval, collapse = "  "), "\n", sep = "")
  cat("- increment:           ", x$increment, "\n", sep = "")
  cat("- Has psi_jac:         ", !is.null(x$psi_jac), "\n", sep = "")
  invisible(x)
}
