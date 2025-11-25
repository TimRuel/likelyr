# ======================================================================
# Nuisance Parameter Specification
# ======================================================================

#' Specify Nuisance Parameter Sampling Strategy
#'
#' @description
#' Defines how **initial guesses** for nuisance parameters (ω̂) are sampled
#' before projection onto the constraint manifold ψ(θ) = ψ̂.
#'
#' This specification provides:
#'
#' * a user-supplied sampler for initial guesses,
#' * a lightweight container for metadata,
#' * validation of the sampler interface.
#'
#' It is used internally by `calibrate_IL()` via:
#' * `make_omega_hat_initgen(cal)`
#' * `make_omega_hat_sampler(cal)`
#'
#' @param init_guess_sampler
#'   A function generating an initial guess vector for nuisance parameters.
#'   Typically something like:
#'   \code{function(scale = 0.25) { ... }}.
#'
#' @param name
#'   Optional character label.
#'
#' @param ...
#'   Additional metadata stored under `$extra`.
#'
#' @return
#' An S3 object of class `"likelihood_nuisance"`.
#'
#' @export
nuisance_spec <- function(
    init_guess_sampler,
    name = NULL,
    ...
) {
  # Construct object
  x <- list(
    name               = name %||% "<nuisance>",
    init_guess_sampler = init_guess_sampler,
    extra              = list(...)
  )

  # Assign class then validate
  class(x) <- "likelihood_nuisance"
  .validate_nuisance_spec(x)
  x
}


# ======================================================================
# INTERNAL VALIDATOR (not exported)
# ======================================================================

#' @keywords internal
.validate_nuisance_spec <- function(x) {

  # init_guess_sampler must be a function
  if (!is.function(x$init_guess_sampler))
    stop("`init_guess_sampler` must be a function.", call. = FALSE)

  # You may later check dimensions, positivity, etc.
  invisible(x)
}


# ======================================================================
# Print method
# ======================================================================

#' @export
print.likelihood_nuisance <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Has init sampler:    ", is.function(x$init_guess_sampler), "\n", sep = "")
  invisible(x)
}
