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
#' @param ...              Additional arguments passed to \code{sieve()},
#'   including \code{rejection_reasons} (see below).
#' @param rejection_reasons Optional character vector of probe rejection
#'   checks to enforce during \code{sieve()}. \code{NULL} (default) enables
#'   all checks. Supply a subset to disable unwanted checks. Recognized
#'   values:
#'   \itemize{
#'     \item \code{"empty_restricted_grid"}
#'     \item \code{"no_feasible_grid_point"}
#'     \item \code{"mode_on_psi_boundary"}
#'     \item \code{"mode_locator_failed"}
#'     \item \code{"mode_eval_failed_after_snap"}
#'     \item \code{"mode_nonfinite"}
#'     \item \code{"mode_infeasible"}
#'     \item \code{"mode_too_low"}
#'     \item \code{"oscillation"}
#'     \item \code{"mode_shift_exhausted"}
#'     \item \code{"jump_left"}
#'     \item \code{"jump_right"}
#'   }
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

  # NOTE: profile() already calibrated model$traversal$max_drop_cap and
  # model$workspace$profile$ll_at_psi_mle from the profile curve (so that
  # probe()/sieve() can run standalone). We deliberately do NOT recompute
  # them here — doing so previously duplicated the exact same logic and
  # invited drift. probe() errors clearly if either is unset.

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
  #
  # Union the profile extent with the accepted branch seed modes so the
  # interval covers branches that peak beyond the profile (see P1).
  # ------------------------------------------------------------------
  psi_interval <- model$estimand$psi_interval %||% NULL

  branch_seeds <- model$workspace$integrated$cache$branch_seeds %||% list()
  branch_modes <- vapply(
    branch_seeds,
    function(s) s$psi_mode %||% NA_real_,
    numeric(1)
  )

  common_interval <- compute_common_interval(
    psi_loglik_df = model$workspace$profile$psi_loglik_df,
    psi_interval = psi_interval,
    increment = model$traversal$increment,
    interval_buffer = model$traversal$interval_buffer %||% 1.0,
    branch_modes = branch_modes
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
