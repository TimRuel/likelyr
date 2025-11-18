#' Nuisance parameter specification
#'
#' @description Defines how nuisance parameter proposals are drawn and then
#' projected into the constraint set ψ(θ) = ψ̂.
#'
#' @param sampler Function(n) → proposal vector.
#' @param init_strategy String: "random", "user", "from_model", "custom".
#' @param name Optional label.
#' @param ... Extra fields.
#'
#' @return S3 object of class `likelihood_nuisance`.
#' @export
nuisance_spec <- function(sampler,
                          init_strategy = c("random", "user", "from_model", "custom"),
                          name = NULL,
                          ...) {

  init_strategy <- match.arg(init_strategy)

  x <- list(
    name                 = name %||% "<nuisance>",
    sampler              = sampler,
    init_strategy        = init_strategy,
    extra                = list(...)
  )
  class(x) <- "likelihood_nuisance"
  validate_nuisance_spec(x)
  x
}

validate_nuisance_spec <- function(x) {
  if (!is.function(x$sampler))
    stop("`sampler` must be a function.", call. = FALSE)

  invisible(x)
}

#' @export
print.likelihood_nuisance <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Init strategy:       ", x$init_strategy, "\n", sep = "")
  invisible(x)
}
