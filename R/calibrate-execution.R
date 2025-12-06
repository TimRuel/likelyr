# ======================================================================
# Execution Calibration
# ======================================================================

#' Calibrate Execution Component
#'
#' @description
#' Execution specifications (serial or parallel) do not depend on data.
#' Calibration simply computes the number of Monte Carlo branches implied
#' by the execution mode and stores it as `$total_branches`.
#'
#' @param exec An execution_spec object.
#'
#' @return The SAME execution_spec object, enriched with:
#'         • $total_branches — integer number of MC branches
#'
#' @export
calibrate_execution <- function(exec) {

  stopifnot(inherits(exec, "execution_spec"))

  # Compute number of branches implied by execution mode
  if (inherits(exec, "serial_spec")) {
    total <- exec$R
  } else if (inherits(exec, "parallel_spec")) {
    total <- exec$num_workers * exec$chunk_size
  } else {
    stop("Unknown execution_spec subtype.", call. = FALSE)
  }

  exec$total_branches <- total

  exec
}
