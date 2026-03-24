# ======================================================================
# likelihood-preprocess.R — Preprocessing Pipeline
# ======================================================================

#' Preprocess Model for Integrated Likelihood
#'
#' @description
#' Orchestrates all preprocessing steps required before
#' \code{integrate()} can be run:
#'
#' \enumerate{
#'   \item \code{profile()} — computes the profile log-likelihood curve,
#'     establishing the extent of the ψ-grid.
#'   \item \code{sieve()} — generates and screens branch seeds.
#'   \item \code{compute_common_interval()} — derives the common ψ
#'     support interval from the profile extent and the distribution of
#'     branch seed modes, intersected with the parameter space boundary
#'     if one exists. This interval is used by all Monte Carlo branches
#'     in \code{generate()} to ensure full overlap and valid CI
#'     estimation.
#' }
#'
#' The common interval is stored on
#' \code{cal$workspace$integrate$common_interval} for use by
#' \code{generate(task = "integrate")}.
#'
#' @param cal     A \code{calibrated} model object.
#' @param c       Positive numeric scalar. Multiplier for the
#'   mode-distribution component of the common interval. Default:
#'   \code{qnorm(1 - alpha_target / 2)} where \code{alpha_target}
#'   is derived from \code{traversal$confidence_levels}.
#' @param verbose Logical. Print diagnostics. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to \code{sieve()}.
#'
#' @return The SAME \code{calibrated} model object, with
#'   \code{cal$workspace$profile} and
#'   \code{cal$workspace$integrate} populated.
#'
#' @export
preprocess <- function(cal, ...) {
  UseMethod("preprocess")
}

#' @export
preprocess.default <- function(cal, ...) {
  stop("preprocess() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
preprocess.calibrated <- function(cal, c = NULL, verbose = FALSE, ...) {
  if (!is_calibrated(cal)) {
    stop("preprocess() requires a calibrated model.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 1. Profile likelihood
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[preprocess] Computing profile likelihood...\n")
  }

  cal <- profile(cal, verbose = verbose)

  if (cal$workspace$profile$status != "success") {
    stop(
      "preprocess(): profile likelihood failed.\n",
      "Check model specification before proceeding.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 2. Sieve
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[preprocess] Running sieve...\n")
  }

  cal <- sieve(cal, verbose = verbose, ...)

  if (verbose) {
    n_accepted <- cal$workspace$integrate$sieve$total_seeds_accepted
    n_requested <- cal$workspace$integrate$sieve$total_seeds_requested
    cat(
      "[preprocess] Sieve complete: ",
      n_accepted,
      "/",
      n_requested,
      " seeds accepted.\n",
      sep = ""
    )
  }

  # ------------------------------------------------------------------
  # 3. Common interval
  # ------------------------------------------------------------------
  psi_interval <- if (!is.null(cal$estimand$psi_interval)) {
    cal$estimand$psi_interval
  } else {
    NULL
  }

  alpha_target <- min(1 - cal$traversal$confidence_levels)

  common_interval <- compute_common_interval(
    profile_psi_ll_df = cal$workspace$profile$psi_ll_df,
    branch_seeds = cal$workspace$integrate$branch_seeds,
    alpha_target = alpha_target,
    psi_interval = psi_interval
  )

  if (verbose) {
    cat(
      "[preprocess] Common interval: [",
      common_interval$psi_lower,
      ", ",
      common_interval$psi_upper,
      "]",
      " | mode_mean = ",
      round(common_interval$mode_mean, 4),
      " | mode_sd = ",
      round(common_interval$mode_sd, 4),
      " | z = ",
      round(common_interval$z, 3),
      "\n",
      sep = ""
    )
  }

  cal$workspace$integrate$common_interval <- common_interval

  cal
}
