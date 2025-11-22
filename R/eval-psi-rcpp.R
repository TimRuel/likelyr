# build_eval_psi_fun <- function(cal) {
#
#   # ---------------------------------------------------------------
#   # Unpack workflow
#   # ---------------------------------------------------------------
#   model     <- cal$workflow$model
#   estimand  <- cal$workflow$estimand
#   optimizer <- cal$workflow$optimizer
#
#   loglik        <- cal$loglik
#   E_loglik      <- cal$E_loglik
#   E_loglik_grad <- cal$E_loglik_grad
#   psi_fn        <- cal$psi_fn
#
#   has_grad <- !is.null(E_loglik_grad)
#
#   # ---------------------------------------------------------------
#   # Rcpp evaluator object
#   # ---------------------------------------------------------------
#   PsiEvaluator <- PsiEvaluatorModule$PsiEvaluator
#
#   evaluator <- PsiEvaluator(
#     E_loglik,
#     if (has_grad) E_loglik_grad else NULL,
#     psi_fn
#   )
#
#   # ---------------------------------------------------------------
#   # Environment storing static + dynamic auglag args
#   # ---------------------------------------------------------------
#   eval_env <- list2env(
#     list(
#       lower       = model$lower,
#       upper       = model$upper,
#       hin         = model$ineq,
#       hinjac      = model$ineq_jac,
#       heqjac      = estimand$psi_jac,
#       localsolver = optimizer$localsolver,
#       localtol    = optimizer$localtol,
#       control     = optimizer$control,
#       deprecatedBehavior = FALSE,
#       x0          = NULL
#     ),
#     parent = emptyenv()
#   )
#
#   # ---------------------------------------------------------------
#   # Permanently registered R wrappers that call C++ methods
#   # ---------------------------------------------------------------
#
#   eval_env$fn <- function(theta) {
#     evaluator$objective(theta)
#   }
#
#   eval_env$gr <- if (has_grad) {
#     function(theta) evaluator$gradient(theta)
#   } else {
#     NULL
#   }
#
#   eval_env$heq <- function(theta) {
#     evaluator$heq(theta)
#   }
#
#   # ---------------------------------------------------------------
#   # Return ω̂ → ψ-evaluation factory
#   # ---------------------------------------------------------------
#   function(omega_hat) {
#
#     # Tell C++ evaluator the new ω̂
#     evaluator$set_omega_hat(omega_hat)
#
#     # -------------------------------------------------------------
#     # Return ψ_target → θ̂(ψ_target, ω̂)
#     # -------------------------------------------------------------
#     function(psi_target, theta_init) {
#
#       evaluator$set_psi_target(psi_target)
#       eval_env$x0 <- theta_init
#
#       theta_hat <- nloptr::auglag(
#         x0          = eval_env$x0,
#         fn          = eval_env$fn,
#         gr          = eval_env$gr,
#         heq         = eval_env$heq,
#         heqjac      = eval_env$heqjac,
#         hin         = eval_env$hin,
#         hinjac      = eval_env$hinjac,
#         lower       = eval_env$lower,
#         upper       = eval_env$upper,
#         localsolver = eval_env$localsolver,
#         localtol    = eval_env$localtol,
#         control     = eval_env$control,
#         deprecatedBehavior = eval_env$deprecatedBehavior
#       )$par
#
#       list(
#         theta_hat  = theta_hat,
#         branch_val = loglik(theta_hat)
#       )
#     }
#   }
# }
