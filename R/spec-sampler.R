# ======================================================================
# spec-sampler.R — Sampler Specification (v2.0)
#
# Governs how omega-hat candidates are generated during screening.
#
# Two optional slots:
#   sampler_fn        — constructor returning function(history) -> omega_hat
#   orbit_expander_fn — constructor returning function(omega_hat) -> list
#
# When sampler_fn is NULL, calibrate_sampler() falls back to the
# built-in gaussian initgen + feasibility projection machinery.
# When orbit_expander_fn is NULL, no orbit expansion is performed.
#
# All model-specific tuning (scales, probabilities, method choice) is
# the responsibility of the user-supplied constructor, not this spec.
# ======================================================================

#' Specify the Omega-Hat Sampling Strategy
#'
#' @description
#' Defines how candidate nuisance parameter vectors (omega-hats) are
#' generated during screening. The spec is intentionally minimal —
#' all sampling logic lives in the constructor functions supplied by
#' the user, not in the spec itself.
#'
#' @section Sampler constructor:
#' \code{sampler_fn} must be a constructor — a function that accepts
#' named arguments from the calibrated model (any subset of
#' \code{param_mle}, \code{param_dim}, \code{psi_fn}, \code{psi_jac},
#' \code{psi_mle}, \code{param_lower}, \code{param_upper},
#' \code{eq_fn}, \code{eq_jac}, \code{ineq_fn}, \code{ineq_jac},
#' \code{solver}) and returns a closure
#' \code{function(history = NULL) -> numeric vector}.
#'
#' The \code{history} argument receives a list of previously accepted
#' omega-hats, which the sampler may use for adaptive exploration.
#' It should be ignored if not needed.
#'
#' When \code{sampler_fn} is \code{NULL}, the built-in gaussian
#' initgen + feasibility projection sampler is used.
#'
#' @section Orbit expander constructor:
#' \code{orbit_expander_fn} must be a constructor accepting the same
#' named arguments as \code{sampler_fn} and returning a closure
#' \code{function(omega_hat) -> list of numeric vectors}.
#' \code{orbit_size} controls how many orbit members are returned per
#' base draw. \code{NULL} means no limit.
#'
#' @param sampler_fn Optional constructor function. When supplied,
#'   replaces the built-in sampling machinery entirely.
#' @param orbit_expander_fn Optional constructor function for orbit
#'   expansion. When supplied, each accepted base omega-hat is expanded
#'   to \code{orbit_size} permuted variants.
#' @param orbit_size Optional positive integer. Number of orbit members
#'   to generate per base draw. \code{NULL} means no limit.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{sampler_spec} object.
#' @export
sampler_spec <- function(
  sampler_fn = NULL,
  orbit_expander_fn = NULL,
  orbit_size = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<sampler>",
    sampler_fn = sampler_fn,
    orbit_expander_fn = orbit_expander_fn,
    orbit_size = orbit_size,
    extra = list(...)
  )

  x <- new_sampler_spec(x)
  .validate_sampler_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_sampler_spec <- function(x) .new_spec(x, "sampler_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_sampler_spec <- function(x) {
  if (!is.null(x$sampler_fn) && !is.function(x$sampler_fn)) {
    stop("sampler_fn must be a function.", call. = FALSE)
  }

  if (!is.null(x$orbit_expander_fn) && !is.function(x$orbit_expander_fn)) {
    stop("orbit_expander_fn must be a function.", call. = FALSE)
  }

  if (!is.null(x$orbit_size)) {
    if (
      !is.numeric(x$orbit_size) ||
        length(x$orbit_size) != 1L ||
        x$orbit_size < 1L
    ) {
      stop("orbit_size must be a positive integer scalar.", call. = FALSE)
    }
    x$orbit_size <- as.integer(x$orbit_size)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.sampler_spec <- function(x, ...) {
  cat("# Sampler Specification\n")
  cat("- Name:              ", x$name, "\n", sep = "")
  cat(
    "- Sampler:            ",
    if (!is.null(x$sampler_fn)) {
      "custom (sampler_fn supplied)"
    } else {
      "built-in default"
    },
    "\n",
    sep = ""
  )
  if (!is.null(x$orbit_expander_fn)) {
    cat(
      "- Orbit expansion:    yes (orbit_size = ",
      x$orbit_size %||% "unlimited",
      ")\n",
      sep = ""
    )
  } else {
    cat("- Orbit expansion:    none\n")
  }
  invisible(x)
}
