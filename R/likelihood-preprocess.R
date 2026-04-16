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
#' @param model            A calibrated \code{model} object.
#' @param verbose          Logical. Print diagnostics. Default: \code{FALSE}.
#' @param ...              Additional arguments passed to \code{sieve()}.
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
preprocess.model <- function(
  model,
  verbose = FALSE,
  ...
) {
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

  # Calibrate absolute drop cap from profile curvature
  profile_df <- model$workspace$profile$psi_loglik_df
  profile_ll <- profile_df$loglik[order(profile_df$psi)]
  profile_drops <- diff(-profile_ll)
  profile_drops <- profile_drops[profile_drops > 0]

  typical_drop <- if (length(profile_drops) > 0L) {
    median(profile_drops)
  } else {
    # fallback: use chi-squared cutoff fraction
    0.5 * qchisq(0.95, df = 1) * 0.05
  }

  model$traversal$max_drop_cap <- model$traversal$cap_multiplier * typical_drop

  model$workspace$profile$ll_at_psi_mle <- max(
    model$workspace$profile$psi_loglik_df$loglik
  )

  # ------------------------------------------------------------------
  # 2. Sieve
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[preprocess] Running sieve...\n")
  }

  t0_sieve <- proc.time()[["elapsed"]]

  model <- sieve(model, verbose = verbose, ...)

  sieve_elapsed <- proc.time()[["elapsed"]] - t0_sieve

  if (verbose) {
    n_accepted <- model$workspace$integrated$cache$total_seeds_accepted
    n_requested <- model$workspace$integrated$cache$total_seeds_requested
    cat(
      "[preprocess] Sieve complete: ",
      n_accepted,
      "/",
      n_requested,
      " seeds accepted in ",
      round(sieve_elapsed, 2),
      "s.\n",
      sep = ""
    )
  }

  # ------------------------------------------------------------------
  # 3. Common interval
  # ------------------------------------------------------------------
  psi_interval <- model$estimand$psi_interval %||% NULL

  common_interval <- compute_common_interval(
    psi_loglik_df = model$workspace$profile$psi_loglik_df,
    psi_interval = psi_interval,
    increment = model$traversal$increment,
    interval_buffer = model$traversal$interval_buffer %||% 1.0
  )

  if (verbose) {
    snap_note <- paste0(
      if (common_interval$snapped_to_lower) " [snapped lower]" else "",
      if (common_interval$snapped_to_upper) " [snapped upper]" else ""
    )
    cat(
      "[preprocess] Common interval: [",
      round(common_interval$psi_lower, 4),
      ", ",
      round(common_interval$psi_upper, 4),
      "]",
      snap_note,
      "\n",
      sep = ""
    )
  }

  model$workspace$integrated$cache$common_interval <- common_interval

  # ------------------------------------------------------------------
  # 4. Store sieve runtime on cache
  # ------------------------------------------------------------------
  model$workspace$integrated$cache$runtime <- list(
    sieve_elapsed = sieve_elapsed
  )

  # ------------------------------------------------------------------
  # 5. Wrap cache as integrated_cache object
  # ------------------------------------------------------------------
  model$workspace$integrated <- new_integrated_cache(
    model$workspace$integrated
  )

  # ------------------------------------------------------------------
  # 6. Mark preprocessed
  # ------------------------------------------------------------------
  model <- mark_preprocessed(model)

  model
}
