# ======================================================================
# Estimand Calibration (v2.2)
#
# Changes from v2.1:
#   • make_psi_fns hook: when present on the estimand_spec, it is called
#     once with data during calibration and its returned psi_fn/psi_jac
#     replace the standard data-bound closures. This allows application
#     specs to precompute data-derived constants (e.g. x_0, J) once
#     rather than on every auglag iteration.
# ======================================================================

#' Calibrate Estimand Component
#'
#' @description
#' Binds data into \code{psi_fn()} and \code{psi_jac()}, computes
#' \code{psi_mle}, optionally sets \code{psi_0} from \code{param_0},
#' and converts bound information into a \code{sets::interval} object
#' stored as \code{psi_domain}.
#'
#' When \code{make_psi_fns} is present on the \code{estimand_spec}, it
#' is called with \code{data} to produce optimized \code{psi_fn} and
#' \code{psi_jac} closures that have data-derived constants already
#' closed over. This replaces the standard data-binding path and avoids
#' recomputing expensive quantities (e.g. the design matrix, reference
#' covariate) on every auglag iteration.
#'
#' If \code{psi_0} is already present on the \code{estimand_spec}
#' (supplied directly by the user), it is respected and not recomputed.
#'
#' @param estimand  An \code{estimand_spec} object.
#' @param data      User data.
#' @param param_mle Numeric vector; MLE of param.
#' @param param_0   Optional numeric vector; true parameter value.
#'
#' @return The same \code{estimand_spec} object, enriched with:
#'   \itemize{
#'     \item \code{$psi_fn}     — data-bound closure \code{function(param)}
#'     \item \code{$psi_jac}    — data-bound closure (if present)
#'     \item \code{$psi_mle}
#'     \item \code{$psi_0}      — if available
#'     \item \code{$psi_domain} — \code{sets::interval} object, or NULL
#'   }
#'
#' @importFrom sets interval
#' @keywords internal
calibrate_estimand <- function(estimand, data, param_mle, param_0 = NULL) {
  stopifnot(inherits(estimand, "estimand_spec"))

  psi_lower <- estimand$psi_lower
  psi_upper <- estimand$psi_upper
  psi_closed <- estimand$psi_closed

  # -------------------------------------------------------------------
  # 1 & 2. Bind data to psi_fn and psi_jac.
  #
  #   When make_psi_fns is present, call it once with data to get
  #   optimized closures with x_0, J, and other constants precomputed.
  #   This replaces the standard binding path for both functions.
  #
  #   When make_psi_fns is absent, use the standard closure approach:
  #   wrap psi_fn(param, data) -> psi_fn(param) by closing over data.
  # -------------------------------------------------------------------
  if (!is.null(estimand$make_psi_fns)) {
    fns <- estimand$make_psi_fns(data)
    estimand$psi_fn <- fns$psi_fn
    estimand$psi_jac <- fns$psi_jac
    estimand$make_psi_fns <- NULL
  } else {
    orig_psi_fn <- estimand$psi_fn
    estimand$psi_fn <- function(param) orig_psi_fn(param, data)

    if (!is.null(estimand$psi_jac)) {
      orig_psi_jac <- estimand$psi_jac
      estimand$psi_jac <- function(param) orig_psi_jac(param, data)
    }
  }

  # -------------------------------------------------------------------
  # 3. Compute psi_mle
  # -------------------------------------------------------------------
  psi_mle <- estimand$psi_fn(param_mle)

  if (!is.numeric(psi_mle) || length(psi_mle) != 1L || !is.finite(psi_mle)) {
    stop("psi_fn(param_mle) must return a finite scalar.", call. = FALSE)
  }

  if (!is.null(psi_lower) && psi_mle < psi_lower) {
    stop("Computed psi_mle lies below psi_lower.", call. = FALSE)
  }

  if (!is.null(psi_upper) && psi_mle > psi_upper) {
    stop("Computed psi_mle lies above psi_upper.", call. = FALSE)
  }

  estimand$psi_mle <- psi_mle

  # -------------------------------------------------------------------
  # 4. Set psi_0 (respect existing value if already supplied by user)
  # -------------------------------------------------------------------
  if (is.null(estimand$psi_0) && !is.null(param_0)) {
    psi_0 <- estimand$psi_fn(param_0)

    if (!is.numeric(psi_0) || length(psi_0) != 1L || !is.finite(psi_0)) {
      stop("psi_fn(param_0) must return a finite scalar.", call. = FALSE)
    }

    if (!is.null(psi_lower) && psi_0 < psi_lower) {
      stop("Computed psi_0 lies below psi_lower.", call. = FALSE)
    }

    if (!is.null(psi_upper) && psi_0 > psi_upper) {
      stop("Computed psi_0 lies above psi_upper.", call. = FALSE)
    }

    estimand$psi_0 <- psi_0
  }

  # -------------------------------------------------------------------
  # 5. Build psi_interval as a sets::interval object
  # -------------------------------------------------------------------
  estimand$psi_interval <- .build_psi_interval(psi_lower, psi_upper, psi_closed)

  estimand$psi_lower <- NULL
  estimand$psi_upper <- NULL
  estimand$psi_closed <- NULL

  estimand
}

# ======================================================================
# INTERNAL: Convert bound slots to sets::interval
# ======================================================================

#' @importFrom sets interval
#' @keywords internal
#' @noRd
.build_psi_interval <- function(psi_lower, psi_upper, psi_closed) {
  if (is.null(psi_lower) && is.null(psi_upper)) {
    return(NULL)
  }

  lower <- psi_lower %||% -Inf
  upper <- psi_upper %||% Inf

  lower_bracket <- if (isTRUE(psi_closed["lower"])) "[" else "("
  upper_bracket <- if (isTRUE(psi_closed["upper"])) "]" else ")"

  bounds <- paste0(lower_bracket, upper_bracket)

  interval(l = lower, r = upper, bounds = bounds)
}
