# ======================================================================
# Integrated Likelihood & Profile Likelihood: GENERATE API
# ======================================================================
# User-facing:
#
#    out <- generate(cal)
#
# Dispatches internally to:
#    generate.IL_calibration()
#    generate.PL_calibration()
#
# Returned objects will have class:
#    "likelihood_integrated"
#    or
#    "likelihood_profile"
#
# ======================================================================


# ----------------------------------------------------------------------
# Top-level generic
# ----------------------------------------------------------------------

generate <- function(cal, ...) {
  UseMethod("generate")
}


# ----------------------------------------------------------------------
# Default (error)
# ----------------------------------------------------------------------

generate.default <- function(cal, ...) {
  stop("`generate()` requires a calibrated workflow object. ",
       "Did you call calibrate(wf, data) first?",
       call. = FALSE)
}


# ======================================================================
# IL: Integrated Likelihood Generation
# ======================================================================
# This is the cleaned-up version of your fit_integrated().
# It relies on:
#   * generate_branches()
#   * average_branches()
#   * cal$il$sample_omega_hat
#   * cal$il$generate_init
#
# ======================================================================

generate.IL_calibration <- function(cal, verbose = TRUE, ...) {

  # ---- Validation ------------------------------------------------------
  if (!inherits(cal, "IL_calibration")) {
    stop("`generate()` for IL requires an `IL_calibration` object.",
         call. = FALSE)
  }

  wf <- cal$workflow

  if (verbose) {
    cat("[generate] Computing Monte Carlo Integrated Likelihood...\n")
  }

  # ---- 1. Branch computation (Monte Carlo IL) --------------------------
  # generate_branches() must:
  #   - sample omega_hats using cal$il$sample_omega_hat
  #   - evaluate branches using build_eval_psi_fun() machinery
  #   - return:
  #         $branches (list of data frames)
  #         $omega_draws (matrix or list of draws)
  branch_result <- generate_branches(
    cal     = cal,
    verbose = verbose,
    ...
  )

  branches    <- branch_result$branches
  omega_draws <- branch_result$omega_draws

  # ---- 2. Average over branches via log-sum-exp ------------------------
  il_curve <- average_branches(branches)

  # ---- 3. Construct output ---------------------------------------------
  out <- list(
    il_curve    = il_curve,
    branches    = branches,
    omega_draws = omega_draws,
    theta_mle   = cal$theta_mle,
    psi_mle     = cal$psi_mle,
    workflow    = wf,
    calibration = cal
  )

  class(out) <- "likelihood_integrated"
  out
}



# ======================================================================
# PL: Profile Likelihood Generation
# ======================================================================
# Placeholder for symmetry with IL.
# Will eventually:
#    - Build profile branches
#    - Evaluate ψ-grid
#    - Return objects of class "likelihood_profile"
# ======================================================================

generate.PL_calibration <- function(cal, verbose = TRUE, ...) {
  stop("Profile likelihood generation not yet implemented.")
}

