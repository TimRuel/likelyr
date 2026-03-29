# ======================================================================
# Internal type predicates
# ======================================================================

#' Check if object is a data.frame
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.is_df <- function(x) {
  is.data.frame(x)
}

# ======================================================================
# Validator: integrate
# ======================================================================

#' Validate integrated likelihood result
#'
#' @param x Result object to validate.
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
#' @noRd
validate_integrated_result <- function(x) {
  if (!is.list(x)) {
    stop("Integrated log-likelihood result must be a list.", call. = FALSE)
  }

  required <- c("status")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Integrated log-likelihood result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!x$status %in% c("success", "failed")) {
    stop("status must be 'success' or 'failed'.", call. = FALSE)
  }

  if (!is.null(x$psi_loglik_df) && !.is_df(x$psi_loglik_df)) {
    stop("psi_loglik_df must be a data.frame.", call. = FALSE)
  }

  invisible(TRUE)
}

# ======================================================================
# Validator: profile
# ======================================================================

#' Validate profile likelihood result
#'
#' @param x Result object to validate.
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
#' @noRd
validate_profile_result <- function(x) {
  if (!is.list(x)) {
    stop("Profile log-likelihood result must be a list.", call. = FALSE)
  }

  required <- c("psi_loglik_df", "psi_hat", "status")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Profile log-likelihood result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!.is_df(x$psi_loglik_df)) {
    stop("psi_loglik_df must be a data.frame.", call. = FALSE)
  }

  if (!is.numeric(x$psi_hat)) {
    stop("psi_hat must be numeric.", call. = FALSE)
  }

  if (!x$status %in% c("success", "failed")) {
    stop("status must be 'success' or 'failed'.", call. = FALSE)
  }

  invisible(TRUE)
}

# ======================================================================
# Validator: diagnostics
# ======================================================================

#' Validate diagnostic result
#'
#' @param x Result object to validate.
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
#' @noRd
validate_diagnostic_result <- function(x) {
  if (!is.list(x)) {
    stop("Diagnostic result must be a list.", call. = FALSE)
  }

  required <- c("supported", "warnings")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Diagnostic result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.logical(x$supported) || length(x$supported) != 1) {
    stop("'supported' must be a single logical.", call. = FALSE)
  }

  if (!is.character(x$warnings)) {
    stop("'warnings' must be a character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

# ======================================================================
# Validator: inference
# ======================================================================

#' Validate inference result
#'
#' @param x Result object to validate.
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
#' @noRd
validate_inference_result <- function(x) {
  if (!is.list(x)) {
    stop("Inference result must be a list.", call. = FALSE)
  }

  required <- c(
    "point_estimate_df",
    "interval_estimate_df"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Inference result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!.is_df(x$point_estimate_df)) {
    stop("point_estimate_df must be a data.frame.", call. = FALSE)
  }

  if (!.is_df(x$interval_estimate_df)) {
    stop("interval_estimate_df must be a data.frame.", call. = FALSE)
  }

  invisible(TRUE)
}

# ======================================================================
# Validator: comparison
# ======================================================================

#' Validate comparison result
#'
#' @param x Result object to validate.
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
#' @noRd
validate_comparison_result <- function(x) {
  if (!is.list(x)) {
    stop("Comparison result must be a list.", call. = FALSE)
  }

  required <- c(
    "point_estimates_df",
    "interval_estimates_df"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Comparison result missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  for (nm in required) {
    if (!.is_df(x[[nm]])) {
      stop(
        nm,
        " must be a data.frame.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
