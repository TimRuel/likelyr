# ======================================================================
# calibrate-execution.R — Execution Calibration (v2.1)
#
# total_seeds lives on the sampler spec and is computed by
# calibrate_sampler(). calibrate_execution() reads it from there and
# uses it to derive chunk_size for parallel execution.
#
# No "seeds" terminology is stored on the execution spec itself.
# ======================================================================

#' Calibrate Execution Component
#'
#' @description
#' Uses \code{sampler$total_seeds} (computed by \code{calibrate_sampler()})
#' to derive execution-level quantities:
#' \itemize{
#'   \item \strong{Serial}: no additional slots needed; \code{total_seeds}
#'     is read directly from the sampler spec at runtime.
#'   \item \strong{Parallel}: derives \code{chunk_size} as the smallest
#'     integer \code{k} such that \code{k * num_workers >= total_seeds},
#'     and stores it on the execution spec.
#' }
#'
#' @param exec    An \code{execution_spec} object.
#' @param sampler A calibrated \code{sampler_spec} object carrying
#'   \code{total_seeds}.
#'
#' @return The SAME \code{execution_spec} object, with \code{chunk_size}
#'   added for parallel specs.
#'
#' @keywords internal
calibrate_execution <- function(exec, sampler) {
  stopifnot(
    inherits(exec, "execution_spec"),
    inherits(sampler, "sampler_spec")
  )

  if (is.null(sampler$total_seeds)) {
    stop(
      "calibrate_execution() requires a calibrated sampler_spec. ",
      "Run calibrate_sampler() first.",
      call. = FALSE
    )
  }

  if (inherits(exec, "parallel_spec")) {
    exec$chunk_size <- as.integer(
      ceiling(sampler$total_seeds / exec$num_workers)
    )
  }

  exec
}
