# ======================================================================
# Optimizer Calibration (v1.2)
# ======================================================================

#' Calibrate Optimizer Component
#'
#' @description
#' Resolves the branch mode locator method specified in
#' \code{optimizer_spec()} to a concrete branch mode locator function,
#' binds all required calibrated objects into its evaluation environment,
#' and attaches the resulting callable to the optimizer specification.
#'
#' The resulting branch mode locator has signature:
#'
#' \preformatted{
#'   function(omega_hat) -> list(
#'     psi_hat,
#'     param_hat,
#'     loglik_at_mode,
#'     status
#'   )
#' }
#'
#' This function is called internally by \code{calibrate()}.
#'
#' @param optimizer A \code{optimizer_spec} object.
#' @param estimand  A calibrated \code{estimand_spec} object.
#' @param parameter A calibrated \code{parameter_spec} object.
#' @param likelihood A calibrated \code{likelihood_spec} object.
#' @param nuisance  A calibrated \code{nuisance_spec} object.
#'
#' @return
#' The SAME \code{optimizer_spec} object, augmented with:
#' \itemize{
#'   \item \code{$branch_mode_locator} — function(omega_hat) → mode object
#' }
#'
#' @keywords internal
calibrate_optimizer <- function(
  optimizer,
  estimand,
  parameter,
  likelihood,
  nuisance
) {
  stopifnot(
    inherits(optimizer, "optimizer_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(nuisance, "nuisance_spec")
  )

  # -------------------------------------------------------------------
  # Resolve locator factory (method already validated)
  # -------------------------------------------------------------------
  locator_factory <- get_branch_mode_locator(
    optimizer$branch_mode_locator_method
  )

  # -------------------------------------------------------------------
  # Build branch function factory
  # (returns branch_fn_factory(omega_hat)(psi, param_init))
  # -------------------------------------------------------------------
  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer
  )

  # -------------------------------------------------------------------
  # Extract calibrated quantities
  # -------------------------------------------------------------------
  psi_init <- estimand$psi_mle
  search_interval <- estimand$search_interval
  # param_init <- parameter$param_mle
  # param_init <- rnorm(length(param_mle))
  psi_jac <- estimand$psi_jac %||% NULL

  # -------------------------------------------------------------------
  # Construct final branch mode locator
  # -------------------------------------------------------------------
  optimizer$branch_mode_locator <- function(omega_hat) {
    # Build ψ-conditional evaluator for this ω̂
    branch_fn <- branch_fn_factory(omega_hat)

    # Environment containing everything the locator needs
    env <- list2env(
      list(
        branch_fn = branch_fn,
        psi_init = psi_init,
        search_interval = search_interval,
        param_init = omega_hat,
        psi_jac = psi_jac,
        .locator_factory = locator_factory
      ),
      parent = environment(locator_factory)
    )

    # Instantiate locator INSIDE env, but resolve factory lexically
    locator <- evalq(.locator_factory(), env)

    environment(locator) <- env

    locator(omega_hat)
  }

  optimizer
}
