# ======================================================================
# probe.R — Omega-hat & Branch-Mode Preflight Diagnostic (Generic)
# ======================================================================

#' Probe Integrated Likelihood Geometry
#'
#' @description
#' Performs a fast preflight check of the omega-hat sampling scheme and
#' branch-mode detection logic. This function draws omega-hat values,
#' detects branch modes, and probes local curvature around each mode.
#'
#' The probe is application-agnostic and relies only on structural
#' interfaces provided by the calibrated model.
#'
#' Intended to be run *before* [integrate()] to assess whether the current
#' model specification is likely to produce well-behaved branches.
#'
#' @param cal A calibrated model object.
#' @param R Integer. Number of omega-hat draws to probe. Default: 50.
#' @param psi_step Numeric. Step size for local probing around psi_hat.
#'   Defaults to `cal$estimand$increment`.
#' @param curvature_tol Numeric. Threshold for acceptable quadratic curvature
#'   (should be negative). Default: -1e-3.
#' @param verbose Logical. Print summary output. Default: TRUE.
#'
#' @return
#' An object of class `"probe"` with components:
#'   • `$summary` — aggregate probe statistics
#'   • `$diagnostics` — per-omega-hat diagnostics (data.frame)
#'   • `$omega_hats` — list of sampled omega-hat values
#'
#' @export
probe <- function(
  cal,
  R = 50L,
  psi_step = NULL,
  curvature_tol = -1e-3,
  verbose = TRUE
) {
  stopifnot(inherits(cal, "calibrated"))

  nuisance <- cal$nuisance
  estimand <- cal$estimand
  optimizer <- cal$optimizer
  parameter <- cal$parameter
  likelihood <- cal$likelihood

  # ------------------------------------------------------------
  # Preconditions
  # ------------------------------------------------------------
  if (
    is.null(nuisance$omega_hat_initgen) ||
      is.null(nuisance$omega_hat_sampler)
  ) {
    stop(
      "probe(): omega-hat functions not found. ",
      "Did you run calibrate()?",
      call. = FALSE
    )
  }

  if (is.null(optimizer$branch_mode_locator)) {
    stop(
      "probe(): optimizer must provide branch_mode_locator().",
      call. = FALSE
    )
  }

  if (is.null(psi_step)) {
    psi_step <- estimand$increment
  }

  # ------------------------------------------------------------
  # Draw omega-hats
  # ------------------------------------------------------------
  initgen <- nuisance$omega_hat_initgen
  sampler <- nuisance$omega_hat_sampler

  omega_hats <- vector("list", R)
  history <- list()

  for (r in seq_len(R)) {
    init <- initgen(history = history)
    omega <- sampler(init)
    omega_hats[[r]] <- omega
    history[[r]] <- omega
  }

  # ------------------------------------------------------------
  # Branch function factory (local use only)
  # ------------------------------------------------------------
  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer
  )

  # ------------------------------------------------------------
  # Diagnostics per omega-hat
  # ------------------------------------------------------------
  diag_list <- vector("list", R)

  for (r in seq_len(R)) {
    omega <- omega_hats[[r]]

    # ---- branch mode detection ----
    mode_obj <- try(
      optimizer$branch_mode_locator(omega),
      silent = TRUE
    )

    if (
      inherits(mode_obj, "try-error") ||
        is.null(mode_obj$psi_hat) ||
        is.null(mode_obj$param_hat) ||
        is.null(mode_obj$loglik_at_mode)
    ) {
      diag_list[[r]] <- data.frame(
        r = r,
        ok = FALSE,
        reason = "mode_not_found",
        curvature = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    psi_hat <- mode_obj$psi_hat
    param_hat <- mode_obj$param_hat
    ll0 <- mode_obj$loglik_at_mode

    # ---- local probing around branch mode ----
    branch_fn <- branch_fn_factory(omega)

    left <- try(
      branch_fn(psi_hat - psi_step, param_hat),
      silent = TRUE
    )
    right <- try(
      branch_fn(psi_hat + psi_step, param_hat),
      silent = TRUE
    )

    if (
      inherits(left, "try-error") ||
        inherits(right, "try-error")
    ) {
      diag_list[[r]] <- data.frame(
        r = r,
        ok = FALSE,
        reason = "local_eval_failed",
        curvature = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    ll_minus <- left$branch_val
    ll_plus <- right$branch_val

    # ---- curvature proxy ----
    curvature <- ll_minus - 2 * ll0 + ll_plus

    is_mode <- (ll0 >= ll_minus && ll0 >= ll_plus)
    curv_ok <- is.finite(curvature) && curvature < curvature_tol

    ok <- is_mode && curv_ok

    diag_list[[r]] <- data.frame(
      r = r,
      ok = ok,
      reason = if (ok) "ok" else "poor_geometry",
      curvature = curvature,
      stringsAsFactors = FALSE
    )
  }

  diagnostics <- do.call(rbind, diag_list)

  # ------------------------------------------------------------
  # Summary
  # ------------------------------------------------------------
  summary <- list(
    R_tested = R,
    frac_ok = mean(diagnostics$ok, na.rm = TRUE),
    frac_curvature_ok = mean(
      diagnostics$curvature < curvature_tol,
      na.rm = TRUE
    )
  )

  if (verbose) {
    cat("# Omega-hat Probe\n")
    cat("Draws tested:           ", summary$R_tested, "\n")
    cat("Overall OK fraction:    ", sprintf("%.2f", summary$frac_ok), "\n")
    cat(
      "Curvature OK fraction:  ",
      sprintf("%.2f", summary$frac_curvature_ok),
      "\n"
    )
  }

  structure(
    list(
      summary = summary,
      diagnostics = diagnostics,
      omega_hats = omega_hats
    ),
    class = "probe"
  )
}

# ======================================================================
# Print method for probe
# ======================================================================

#' @export
print.probe <- function(x, ...) {
  stopifnot(inherits(x, "probe"))

  summary <- x$summary
  diag <- x$diagnostics

  cat("# Omega-hat Probe\n\n")

  cat("Draws tested:        ", summary$R_tested, "\n", sep = "")
  cat(
    "Overall OK fraction: ",
    sprintf("%.2f", summary$frac_ok),
    "\n",
    sep = ""
  )
  cat(
    "Curvature OK frac:   ",
    sprintf("%.2f", summary$frac_curvature_ok),
    "\n",
    sep = ""
  )

  # --------------------------------------------------
  # Failure breakdown
  # --------------------------------------------------
  if (!all(diag$ok)) {
    cat("\nFailure breakdown:\n")

    tab <- table(diag$reason)
    for (nm in names(tab)) {
      cat(
        "  - ",
        nm,
        ": ",
        tab[[nm]],
        "\n",
        sep = ""
      )
    }
  }

  # --------------------------------------------------
  # Heuristic guidance
  # --------------------------------------------------
  if (summary$frac_ok < 0.7) {
    cat(
      "\n⚠️  Warning: Low fraction of well-behaved branch modes.\n",
      "   Consider tightening omega-hat sampling or adding barriers.\n",
      sep = ""
    )
  } else if (summary$frac_ok < 0.9) {
    cat(
      "\nℹ️  Moderate branch quality.\n",
      "   integrate() may work, but expect some discarded branches.\n",
      sep = ""
    )
  } else {
    cat(
      "\n✓ Omega-hat sampling appears healthy.\n",
      sep = ""
    )
  }

  invisible(x)
}

# ======================================================================
# Plot method for probe
# ======================================================================

#' @export
plot.probe <- function(x, ...) {
  stopifnot(inherits(x, "probe"))

  diag <- x$diagnostics

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mfrow = c(1, 3))

  # --------------------------------------------------
  # 1. Curvature histogram
  # --------------------------------------------------
  curv <- diag$curvature[is.finite(diag$curvature)]

  hist(
    curv,
    breaks = 30,
    col = "grey80",
    border = "white",
    main = "Local curvature\n(ℓ_- − 2ℓ₀ + ℓ_+)",
    xlab = "Curvature"
  )
  abline(v = 0, col = "red", lty = 2)

  # --------------------------------------------------
  # 2. Curvature vs draw index
  # --------------------------------------------------
  plot(
    diag$r,
    diag$curvature,
    pch = 19,
    col = ifelse(diag$ok, "black", "red"),
    main = "Curvature by ω̂ draw",
    xlab = "Draw index",
    ylab = "Curvature"
  )
  abline(h = 0, col = "red", lty = 2)

  # --------------------------------------------------
  # 3. OK vs not OK
  # --------------------------------------------------
  boxplot(
    curvature ~ ok,
    data = diag,
    names = c("Fail", "OK"),
    col = c("grey85", "grey60"),
    main = "Curvature by branch quality",
    ylab = "Curvature"
  )
  abline(h = 0, col = "red", lty = 2)

  invisible(x)
}
