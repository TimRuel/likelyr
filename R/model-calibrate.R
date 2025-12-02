# =====================================================================
# Calibration
# =====================================================================

#' Calibrate a model_spec to data
#'
#' @description
#' Performs all preprocessing steps needed before computing
#' integrated or profile likelihoods:
#'
#'   • binding data to likelihood and estimand closures
#'   • computing theta_MLE
#'   • computing psi_MLE
#'   • computing branch-mode search interval
#'   • carrying forward model-level constraints
#'
#' @param model  A model_spec object.
#' @param data   User data.
#' @param verbose Logical; print diagnostics.
#'
#' @return A `calibrated_model` object.
#' @export
calibrate <- function(model, data, verbose = TRUE) {
  UseMethod("calibrate")
}

#' @export
calibrate.default <- function(model, data, verbose = TRUE) {
  stop("calibrate() currently only supports objects of class 'model_spec'.",
       call. = FALSE)
}

#' @export
calibrate.model_spec <- function(model, data, verbose = TRUE) {

  .validate_model_for_calibration(model)

  lik      <- model$likelihood
  estimand <- model$estimand
  nuisance <- model$nuisance
  opt      <- model$optimizer
  exec     <- model$execution

  # -------------------------------------------------------------------
  # 1. Bind data → closures
  # -------------------------------------------------------------------
  loglik <- function(theta) lik$loglik(theta, data)

  psi_fn <- function(theta) estimand$psi_fn(theta, data)

  psi_jac <- if (!is.null(estimand$psi_jac)) {
    function(theta) estimand$psi_jac(theta, data)
  } else NULL

  if (!is.null(nuisance)) {
    E_loglik <- function(theta, omega_hat) {
      nuisance$E_loglik(theta, omega_hat, data)
    }
    E_loglik_grad <- if (!is.null(nuisance$E_loglik_grad)) {
      function(theta, omega_hat) nuisance$E_loglik_grad(theta, omega_hat, data)
    } else NULL
  } else {
    E_loglik      <- NULL
    E_loglik_grad <- NULL
  }

  # -------------------------------------------------------------------
  # 2. Compute theta_MLE and psi_MLE
  # -------------------------------------------------------------------
  theta_mle_fn <- lik$theta_mle_fn
  theta_mle    <- theta_mle_fn(data)

  theta_dim <- lik$theta_dim
  if (length(theta_mle) != theta_dim) {
    stop(
      sprintf(
        "theta_mle_fn(data) returned length %d but theta_dim = %d.",
        length(theta_mle), theta_dim
      ),
      call. = FALSE
    )
  }

  psi_mle <- psi_fn(theta_mle)

  if (!is.numeric(psi_mle) || length(psi_mle) != 1L || !is.finite(psi_mle)) {
    stop("psi_fn(theta_mle, data) must return a finite numeric scalar.",
         call. = FALSE)
  }

  # -------------------------------------------------------------------
  # 3. Compute branch-mode search interval
  # -------------------------------------------------------------------
  si_fn <- estimand$search_interval_fn
  si    <- si_fn(data)

  if (!is.numeric(si) || length(si) != 2L || any(!is.finite(si)) || si[1] >= si[2]) {
    stop(
      "search_interval_fn(data) must return c(lower, upper) with finite lower < upper.",
      call. = FALSE
    )
  }

  if (verbose) {
    cat("[calibrate] theta_MLE = (",
        paste(format(theta_mle), collapse = ", "),
        ")\n", sep = "")
    cat("[calibrate] psi_MLE   = ", psi_mle, "\n", sep = "")
    cat("[calibrate] interval  = [", si[1], ", ", si[2], "]\n", sep = "")
  }

  # -------------------------------------------------------------------
  # 4. Construct calibrated_model object
  # -------------------------------------------------------------------
  out <- list(
    # Raw spec components
    likelihood = lik,
    estimand   = estimand,
    nuisance   = nuisance,
    optimizer  = opt,
    execution  = exec,

    # User data
    data = data,

    # MLE quantities
    theta_mle = theta_mle,
    psi_mle   = psi_mle,

    # Bound closures
    loglik        = loglik,
    psi_fn        = psi_fn,
    psi_jac       = psi_jac,
    E_loglik      = E_loglik,
    E_loglik_grad = E_loglik_grad,

    # Estimand metadata
    search_interval   = si,
    increment         = estimand$increment,
    confidence_levels = estimand$confidence_levels
  )

  class(out) <- "calibrated_model"
  out
}

# =====================================================================
# INTERNAL VALIDATION
# =====================================================================

.validate_model_for_calibration <- function(model) {

  if (!inherits(model$likelihood, "likelihood_spec"))
    stop("model$likelihood must be a likelihood_spec().", call. = FALSE)

  if (!inherits(model$estimand, "estimand_spec"))
    stop("model$estimand must be an estimand_spec().", call. = FALSE)

  if (!inherits(model$optimizer, "optimizer_spec"))
    stop("model$optimizer must be an optimizer_spec().", call. = FALSE)

  if (!inherits(model$execution, "execution_spec"))
    stop("model$execution must be an execution_spec().", call. = FALSE)

  if (!is.null(model$nuisance) &&
      !inherits(model$nuisance, "nuisance_spec"))
    stop("model$nuisance must be nuisance_spec() or NULL.", call. = FALSE)

  invisible(model)
}
