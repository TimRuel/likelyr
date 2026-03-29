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
#'   \item \code{profile()} — computes the profile log-likelihood curve.
#'   \item \code{sieve()} — generates and screens branch seeds.
#'   \item \code{compute_common_interval()} — derives the common ψ
#'     support interval from the profile extent and the distribution of
#'     branch seed modes, intersected with the parameter space boundary
#'     if one exists.
#' }
#'
#' The common interval is stored on
#' \code{model$workspace$integrated$cache$common_interval} for use by
#' \code{generate(task = "integrate")}.
#'
#' @param model     A calibrated \code{model} object.
#' @param z       Positive numeric scalar. Multiplier for the
#'   mode-distribution component of the common interval. Default:
#'   \code{qnorm(1 - alpha_target / 2)}.
#' @param verbose Logical. Print diagnostics. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to \code{sieve()}.
#'
#' @return The same calibrated \code{model} object with
#'   \code{model$workspace$profile} and
#'   \code{model$workspace$integrated} populated, marked as
#'   preprocessed.
#'
#' @export
preprocess <- function(model, ...) {
  UseMethod("preprocess")
}

#' @export
preprocess.default <- function(model, ...) {
  stop("preprocess() requires a calibrated 'model' object.", call. = FALSE)
}

#' @export
preprocess.model <- function(model, verbose = FALSE, ...) {
  if (!is_calibrated(model)) {
    stop("preprocess() requires a calibrated model.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 1. Profile likelihood
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[preprocess] Computing profile likelihood...\n")
  }

  model <- profile(model, verbose = verbose)

  if (model$workspace$profile$status != "success") {
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

  model <- sieve(model, verbose = verbose, ...)

  if (verbose) {
    n_accepted <- model$workspace$integrated$cache$total_seeds_accepted
    n_requested <- model$workspace$integrated$cache$total_seeds_requested
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
  psi_interval <- model$estimand$psi_interval %||% NULL
  alpha_target <- min(1 - model$traversal$confidence_levels)

  common_interval <- compute_common_interval(
    psi_loglik_df = model$workspace$profile$psi_loglik_df,
    branch_seeds = model$workspace$integrated$cache$branch_seeds,
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

  model$workspace$integrated$cache$common_interval <- common_interval

  # ------------------------------------------------------------------
  # 4. Wrap cache as integrated_cache object
  # ------------------------------------------------------------------
  model$workspace$integrated <- new_integrated_cache(
    model$workspace$integrated
  )

  # ------------------------------------------------------------------
  # 5. Mark preprocessed
  # ------------------------------------------------------------------
  model <- mark_preprocessed(model)

  model
}
