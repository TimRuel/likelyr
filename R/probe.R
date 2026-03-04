# ======================================================================
# probe.R — Omega-hat & Branch-Mode Preflight Diagnostic (Generic)
# ======================================================================

#' Probe Integrated Likelihood Geometry
#'
#' @description
#' Fast preflight check of omega-hat sampling + branch-mode behavior.
#' Draws omega-hat values, locates branch modes, and probes local shape
#' at psi_hat ± psi_step.
#'
#' This function is application-agnostic and relies only on structural
#' interfaces provided by the calibrated model.
#'
#' @param cal A calibrated model object.
#' @param R Integer. Number of omega-hat draws to probe. Default: 50.
#' @param psi_step Numeric. Step size for local probing around psi_hat.
#'   Defaults to `cal$estimand$increment`.
#' @param curvature_tol Numeric. Threshold for acceptable quadratic curvature
#'   (should be negative). Default: -1e-3.
#' @param mode_tol Numeric. Tolerance for comparing adjacent values to the
#'   alleged mode. Default: 1e-8.
#' @param flat_tol Numeric. Tolerance for treating curvature as "flat".
#'   Default: 1e-10.
#' @param verbose Logical. Print summary output. Default: TRUE.
#'
#' @return A `"probe"` object with $summary, $diagnostics, $omega_hats.
#' @export
probe <- function(
  cal,
  R = 50L,
  psi_step = NULL,
  curvature_tol = -1e-3,
  mode_tol = 1e-8,
  flat_tol = 1e-10,
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
  # new — legacy fallback
  if (
    !is.null(nuisance$omega_hat_initgen) && !is.null(nuisance$omega_hat_sampler)
  ) {
    initgen <- nuisance$omega_hat_initgen
    sampler <- nuisance$omega_hat_sampler
  } else {
    initgen <- make_omega_hat_initgen(cal)
    sampler <- make_omega_hat_sampler(cal)
  }

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
  # Helper: safe extraction of optional diagnostics
  # ------------------------------------------------------------
  .get <- function(x, nm) {
    if (is.list(x) && !is.null(x[[nm]])) x[[nm]] else NA
  }

  # ------------------------------------------------------------
  # Diagnostics per omega-hat
  # ------------------------------------------------------------
  diag_list <- vector("list", R)

  for (r in seq_len(R)) {
    omega <- omega_hats[[r]]

    # ---- branch mode detection ----
    mode_obj <- try(optimizer$branch_mode_locator(omega), silent = TRUE)

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
        ll_minus = NA_real_,
        ll0 = NA_real_,
        ll_plus = NA_real_,
        curvature = NA_real_,
        mode_violation = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    psi_hat <- mode_obj$psi_hat
    param_hat <- mode_obj$param_hat
    ll0 <- mode_obj$loglik_at_mode

    # ---- local probing around branch mode ----
    branch_fn <- branch_fn_factory(omega)

    left <- try(branch_fn(psi_hat - psi_step, param_hat), silent = TRUE)
    right <- try(branch_fn(psi_hat + psi_step, param_hat), silent = TRUE)

    if (inherits(left, "try-error") || inherits(right, "try-error")) {
      diag_list[[r]] <- data.frame(
        r = r,
        ok = FALSE,
        reason = "local_eval_failed",
        ll_minus = NA_real_,
        ll0 = ll0,
        ll_plus = NA_real_,
        curvature = NA_real_,
        mode_violation = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    ll_minus <- left$branch_val
    ll_plus <- right$branch_val

    # ---- primary geometry diagnostics (always available) ----
    curvature <- ll_minus - 2 * ll0 + ll_plus
    is_mode <- (ll0 + mode_tol >= ll_minus) && (ll0 + mode_tol >= ll_plus)

    # Maximum amount by which neighbors exceed the alleged mode
    mode_violation <- max(ll_minus - ll0, ll_plus - ll0, na.rm = TRUE)

    curv_finite <- is.finite(curvature)
    curv_ok <- curv_finite && curvature < curvature_tol

    ok <- is_mode && curv_ok

    # --------------------------------------------------------
    # Tier-1 classification (always possible)
    # --------------------------------------------------------
    # 1) If neighbor exceeds the mode -> mode inconsistency
    # 2) Else if curvature >= 0 or ~0 -> flat/no interior mode
    # 3) Else -> poor_geometry (e.g., curvature negative but too small)
    reason <- "ok"
    if (!is_mode) {
      reason <- "mode_inconsistent"
    } else if (!curv_finite) {
      reason <- "nonfinite_curvature"
    } else if (curvature >= -flat_tol) {
      reason <- "flat_or_no_mode"
    } else if (!curv_ok) {
      reason <- "weak_curvature"
    }

    # --------------------------------------------------------
    # Tier-2 refinement (if branch_fn returns diagnostics)
    # --------------------------------------------------------
    # These fields will be NA unless your branch factory provides them.
    # If present, they can explain *why* geometry is poor.
    left_bnd <- .get(left, "bound_min_slack")
    right_bnd <- .get(right, "bound_min_slack")
    left_hin <- .get(left, "ineq_max")
    right_hin <- .get(right, "ineq_max")
    left_eq <- .get(left, "eq_resid_inf")
    right_eq <- .get(right, "eq_resid_inf")

    left_stat <- .get(left, "solver_status")
    right_stat <- .get(right, "solver_status")

    # Heuristic flags (agnostic):
    # - constraint kink: one side hits a bound / violates inequality much more
    # - solver instability: status differs or residuals spike
    constraint_kink <- FALSE
    solver_instability <- FALSE

    if (is.finite(left_bnd) && is.finite(right_bnd)) {
      # one side much closer to a bound than the other
      if (min(left_bnd, right_bnd) < 1e-8 && abs(left_bnd - right_bnd) > 1e-6) {
        constraint_kink <- TRUE
      }
    }

    if (is.finite(left_hin) && is.finite(right_hin)) {
      # one side substantially more infeasible than the other
      if (max(left_hin, right_hin) > 1e-6 && abs(left_hin - right_hin) > 1e-6) {
        constraint_kink <- TRUE
      }
    }

    if (!is.na(left_stat) && !is.na(right_stat) && left_stat != right_stat) {
      solver_instability <- TRUE
    }

    if (is.finite(left_eq) && is.finite(right_eq)) {
      if (max(left_eq, right_eq) > 1e-6 && abs(left_eq - right_eq) > 1e-6) {
        solver_instability <- TRUE
      }
    }

    # Refine reason only if we already have a failure
    if (!ok) {
      if (
        reason %in% c("mode_inconsistent", "weak_curvature", "flat_or_no_mode")
      ) {
        if (constraint_kink) {
          reason <- paste0(reason, "|constraint_kink")
        }
        if (solver_instability) {
          reason <- paste0(reason, "|solver_instability")
        }
      }
    }

    diag_list[[r]] <- data.frame(
      r = r,
      ok = ok,
      reason = reason,
      ll_minus = ll_minus,
      ll0 = ll0,
      ll_plus = ll_plus,
      curvature = curvature,
      mode_violation = mode_violation,

      # optional diagnostics (NA if unavailable)
      left_bound_min_slack = left_bnd,
      right_bound_min_slack = right_bnd,
      left_ineq_max = left_hin,
      right_ineq_max = right_hin,
      left_eq_resid_inf = left_eq,
      right_eq_resid_inf = right_eq,
      left_solver_status = left_stat,
      right_solver_status = right_stat,

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
    ),
    frac_mode_inconsistent = mean(
      diagnostics$reason == "mode_inconsistent",
      na.rm = TRUE
    ),
    frac_flat_or_no_mode = mean(
      grepl("^flat_or_no_mode", diagnostics$reason),
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
    cat(
      "Mode inconsistent frac: ",
      sprintf("%.2f", summary$frac_mode_inconsistent),
      "\n"
    )
    cat(
      "Flat/no-mode frac:      ",
      sprintf("%.2f", summary$frac_flat_or_no_mode),
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
# Print method for probe (enhanced diagnostics)
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

  if (!is.null(summary$frac_mode_inconsistent)) {
    cat(
      "Mode inconsistent:   ",
      sprintf("%.2f", summary$frac_mode_inconsistent),
      "\n",
      sep = ""
    )
  }

  if (!is.null(summary$frac_flat_or_no_mode)) {
    cat(
      "Flat / no mode:      ",
      sprintf("%.2f", summary$frac_flat_or_no_mode),
      "\n",
      sep = ""
    )
  }

  # --------------------------------------------------
  # Failure breakdown
  # --------------------------------------------------
  if (!all(diag$ok)) {
    cat("\nFailure breakdown:\n")

    tab <- sort(table(diag$reason), decreasing = TRUE)
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
  # Severity diagnostics (if available)
  # --------------------------------------------------
  if ("mode_violation" %in% names(diag)) {
    bad <- diag$mode_violation[!diag$ok & is.finite(diag$mode_violation)]
    if (length(bad) > 0) {
      cat(
        "\nMode violation (max adjacent excess):\n",
        "  median = ",
        signif(stats::median(bad), 3),
        ", max = ",
        signif(max(bad), 3),
        "\n",
        sep = ""
      )
    }
  }

  # --------------------------------------------------
  # Heuristic guidance
  # --------------------------------------------------
  if (summary$frac_ok < 0.5) {
    cat(
      "\n❌ Omega-hat sampling frequently induces ill-defined branches.\n",
      "   Many omega-hats may not admit an interior branch mode.\n",
      sep = ""
    )
  } else if (summary$frac_ok < 0.7) {
    cat(
      "\n⚠️  Warning: Low fraction of well-behaved branch modes.\n",
      "   Expect many branches to be discarded during aggregation.\n",
      sep = ""
    )
  } else if (summary$frac_ok < 0.9) {
    cat(
      "\nℹ️  Moderate branch quality.\n",
      "   integrate() may work, but diagnostics are recommended.\n",
      sep = ""
    )
  } else {
    cat(
      "\n✓ Omega-hat sampling appears geometrically stable.\n",
      sep = ""
    )
  }

  invisible(x)
}

# ======================================================================
# Plot method for probe (enhanced geometry diagnostics)
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
  # 2. Mode violation vs curvature
  # --------------------------------------------------
  if ("mode_violation" %in% names(diag)) {
    mv <- diag$mode_violation
    plot(
      mv,
      diag$curvature,
      pch = 19,
      col = ifelse(diag$ok, "black", "red"),
      main = "Mode violation vs curvature",
      xlab = "Max(ℓ_adj − ℓ_mode)",
      ylab = "Curvature"
    )
    abline(v = 0, col = "grey60", lty = 2)
    abline(h = 0, col = "grey60", lty = 2)
  } else {
    plot.new()
    text(0.5, 0.5, "mode_violation\nnot available")
  }

  # --------------------------------------------------
  # 3. Geometry by outcome class
  # --------------------------------------------------
  cls <- factor(
    ifelse(
      diag$ok,
      "OK",
      sub("\\|.*$", "", diag$reason)
    )
  )

  boxplot(
    diag$curvature ~ cls,
    las = 2,
    col = "grey80",
    main = "Curvature by branch outcome",
    ylab = "Curvature"
  )
  abline(h = 0, col = "red", lty = 2)

  invisible(x)
}
