# ----------------------------------------------------------------------
# Internal type predicates
# ----------------------------------------------------------------------

.is_df <- function(x) {
  is.data.frame(x)
}

.is_plot <- function(x) {
  inherits(x, "ggplot")
}

.is_table <- function(x) {
  inherits(x, "knitr_kable") || inherits(x, "gt_tbl")
}

# ======================================================================
# Validator: integrate
# ======================================================================

validate_integrate_result <- function(x) {

  if (!is.list(x))
    stop("Integrated log-likelihood result must be a list.", call. = FALSE)

  required <- c("status")

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Integrated log-likelihood result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!x$status %in% c("success", "failed"))
    stop("status must be 'success' or 'failed'.", call. = FALSE)

  if (!is.null(x$psi_ll_df) && !is.data.frame(x$psi_ll_df))
    stop("psi_ll_df must be a data.frame.", call. = FALSE)

  if (!is.null(x$pseudolikelihood_points) &&
      !inherits(x$pseudolikelihood_points, "ggplot"))
    stop("pseudolikelihood_points must be a ggplot object if present.", call. = FALSE)

  invisible(TRUE)
}


# ======================================================================
# Validator: profile
# ======================================================================

validate_profile_result <- function(x) {

  if (!is.list(x))
    stop("Profile log-likelihood result must be a list.", call. = FALSE)

  required <- c("psi_ll_df", "psi_mle", "theta_mle", "status")

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Profile log-likelihood result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(x$psi_ll_df) && !is.data.frame(x$psi_ll_df))
    stop("psi_ll_df must be a data.frame.", call. = FALSE)

  if (!is.numeric(x$psi_mle))
    stop("psi_mle must be numeric.", call. = FALSE)

  if (!is.numeric(x$theta_mle))
    stop("theta_mle must be numeric.", call. = FALSE)

  if (!x$status %in% c("success", "failed"))
    stop("status must be 'success' or 'failed'.", call. = FALSE)

  if (!is.null(x$profile_plot) &&
      !inherits(x$profile_plot, "ggplot"))
    stop("pseudolikelihood_points must be a ggplot if present.", call. = FALSE)

  invisible(TRUE)
}

# ----------------------------------------------------------------------
# Validator: diagnostics
# ----------------------------------------------------------------------

validate_diagnostics_result <- function(x) {

  if (!is.list(x))
    stop("Diagnostics result must be a list.", call. = FALSE)

  required <- c("supported", "warnings")
  missing  <- setdiff(required, names(x))

  if (length(missing)) {
    stop(
      "Diagnostics result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.logical(x$supported) || length(x$supported) != 1)
    stop("'supported' must be a single logical.", call. = FALSE)

  if (!is.character(x$warnings))
    stop("'warnings' must be a character vector.", call. = FALSE)

  invisible(TRUE)
}

# ======================================================================
# Validator: inference
# ======================================================================

validate_inference_result <- function(x) {

  if (!is.list(x)) {
    stop("Inference result must be a list.", call. = FALSE)
  }

  # --------------------------------------------------
  # Required fields
  # --------------------------------------------------
  required <- c("psi_ll_df", "estimate_df", "estimate_table")

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Inference result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Type checks
  # --------------------------------------------------
  if (!.is_df(x$psi_ll_df)) {
    stop("psi_ll_df must be a data.frame.", call. = FALSE)
  }

  if (!.is_df(x$estimate_df)) {
    stop("estimate_df must be a data.frame.", call. = FALSE)
  }

  if (!.is_table(x$estimate_table)) {
    stop(
      "estimate_table must be an HTML table (knitr_kable or gt_tbl).",
      call. = FALSE
    )
  }

  if (!is.null(x$pseudolikelihood_curve) &&
      !.is_plot(x$pseudolikelihood_curve)) {
    stop(
      "pseudolikelihood_curve must be a ggplot object if present.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ======================================================================
# Validator: comparison
# ======================================================================

validate_comparison_result <- function(x) {

  if (!is.list(x)) {
    stop("Comparison result must be a list.", call. = FALSE)
  }

  if (!"tables" %in% names(x)) {
    stop("Comparison result must contain a 'tables' element.", call. = FALSE)
  }

  if (!is.list(x$tables)) {
    stop("'tables' must be a list.", call. = FALSE)
  }

  if (is.null(names(x$tables))) {
    stop("'tables' must be a named list.", call. = FALSE)
  }

  # --------------------------------------------------
  # Validate table contents
  # --------------------------------------------------
  for (nm in names(x$tables)) {

    obj <- x$tables[[nm]]

    if (!.is_df(obj) && !.is_table(obj)) {
      stop(
        "tables[['", nm,
        "']] must be a data.frame or HTML table.",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------
  # Plot validation
  # --------------------------------------------------
  if (!is.null(x$pseudolikelihood_curves) && !.is_plot(x$pseudolikelihood_curves)) {
    stop("pseudolikelihood_curves must be a ggplot object if present.", call. = FALSE)
  }

  invisible(TRUE)
}

