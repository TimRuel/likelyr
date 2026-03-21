# ======================================================================
# Execution Calibration (v2.0)
#
# Changes from v1.0:
#   • R replaced by min_branches and branch_buffer on both spec types
#   • serial_spec: stores total_seeds = min_branches + branch_buffer
#   • parallel_spec: derives chunk_size = ceiling((min_branches +
#     branch_buffer) / num_workers), stores chunk_size and total_seeds
# ======================================================================

#' Calibrate Execution Component
#'
#' @description
#' Derives and stores computed quantities from the execution spec:
#' \itemize{
#'   \item \strong{Serial}: stores \code{total_seeds =
#'     min_branches + branch_buffer}.
#'   \item \strong{Parallel}: derives \code{chunk_size} as the smallest
#'     integer \code{k} such that \code{k * num_workers >=
#'     min_branches + branch_buffer}, then stores \code{chunk_size} and
#'     \code{total_seeds = num_workers * chunk_size}.
#' }
#'
#' @param exec An \code{execution_spec} object.
#'
#' @return The SAME \code{execution_spec} object, enriched with:
#'   \itemize{
#'     \item \code{$total_seeds}  — integer; total seeds to request from
#'       \code{sieve()}
#'     \item \code{$chunk_size}   — integer (parallel only); tasks per
#'       worker
#'   }
#'
#' @keywords internal
calibrate_execution <- function(exec) {
  stopifnot(inherits(exec, "execution_spec"))

  if (inherits(exec, "serial_spec")) {
    exec$total_seeds <- as.integer(exec$min_branches + exec$branch_buffer)
  } else if (inherits(exec, "parallel_spec")) {
    target <- exec$min_branches + exec$branch_buffer
    chunk_size <- as.integer(ceiling(target / exec$num_workers))
    exec$chunk_size <- chunk_size
    exec$total_seeds <- as.integer(exec$num_workers * chunk_size)
  } else {
    stop("Unknown execution_spec subtype.", call. = FALSE)
  }

  exec
}
