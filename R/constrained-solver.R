# ======================================================================
# Constrained solver factory: maximize E_loglik subject to psi(theta)=psi_target
# ======================================================================

#' Build a constrained theta-solver for a calibrated workflow
#'
#' @description
#' Internal helper. Given a `likelihood_calibration` object, construct a function
#' that, for any `(psi_target, omega_hat)`, solves
#'
#'   argmax_theta  E_loglik(theta, omega_hat)
#'   subject to    psi(theta) = psi_target
#'
#' using the optimizer specified in `cal$workflow$optimizer`.
#'
#' @param cal A `likelihood_calibration` object from [calibrate()].
#'
#' @return A function with signature
#'   `solve_theta(psi_target, omega_hat, theta_init = cal$theta_hat)`
#'   which returns a list with elements `theta_hat` and `opt_obj`.
#'
#' @keywords internal
.make_constrained_solver <- function(cal) {

  wf        <- cal$workflow
  optimizer <- wf$optimizer
  model     <- wf$model
  estimand  <- wf$estimand

  use_grad <- isTRUE(optimizer$use_grad) &&
    !is.null(model$grad) &&
    !is.null(estimand$dpsi)

  # The returned function solves for theta given psi_target and omega_hat
  function(psi_target,
           omega_hat,
           theta_init = cal$theta_hat) {

    # Objective: maximize E_loglik => minimize -E_loglik
    obj <- function(theta) {
      -cal$E_loglik(theta, omega_hat)
    }

    # Gradient of objective if available: ∇[-E_loglik] = -∇E_loglik
    grad_obj <- if (use_grad) {
      function(theta) -model$grad(theta, omega_hat)
    } else {
      NULL
    }

    # Equality constraint: psi(theta) - psi_target = 0
    heq <- function(theta) {
      cal$psi(theta) - psi_target
    }

    # Jacobian of heq: ∂psi/∂theta (row vector)
    jac_heq <- if (use_grad) {
      function(theta) {
        # ensure 1 x p matrix
        as.matrix(estimand$dpsi(theta), nrow = 1L)
      }
    } else {
      NULL
    }

    # Dispatch on optimizer method
    if (optimizer$method == "nloptr") {

      # nloptr::auglag interface: fn, gr, heq, jac, control
      opt <- nloptr::auglag(
        x0   = theta_init,
        fn   = obj,
        gr   = grad_obj,
        heq  = heq,
        jac  = jac_heq,
        control = optimizer$control %||% list()
      )

      theta_hat <- opt$par
      opt_obj   <- opt

    } else if (optimizer$method == "optim") {

      # For optim, handle constraint via a penalty (simple baseline)
      # NOTE: This is a crude fallback; serious use should prefer nlopt.
      pen_obj <- function(theta) {
        value    <- obj(theta)
        penalty  <- sum(heq(theta)^2) * 1e4
        value + penalty
      }

      opt <- stats::optim(
        par     = theta_init,
        fn      = pen_obj,
        control = optimizer$control %||% list()
      )

      theta_hat <- opt$par
      opt_obj   <- opt

    } else {
      stop("Unsupported optimizer method for constrained solve: ",
           optimizer$method, call. = FALSE)
    }

    list(
      theta_hat = theta_hat,
      opt_obj   = opt_obj
    )
  }
}
