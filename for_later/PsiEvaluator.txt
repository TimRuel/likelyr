#include <Rcpp.h>
using namespace Rcpp;

// This module encapsulates model-specific evaluation logic
class PsiEvaluator {
public:
  NumericVector omega_hat;  // copied from R
  NumericVector psi_target;

  Function E_loglik;
  Function E_loglik_grad;
  Function psi_fn;

  PsiEvaluator(Function E_loglik_,
               Function E_loglik_grad_,
               Function psi_fn_) :
    E_loglik(E_loglik_),
    E_loglik_grad(E_loglik_grad_),
    psi_fn(psi_fn_) {}

  void set_omega_hat(NumericVector om) {
    omega_hat = clone(om);
  }

  void set_psi_target(NumericVector psi_t) {
    psi_target = clone(psi_t);
  }

  // Objective: -E_loglik(theta, omega_hat)
  double objective(NumericVector theta) {
    NumericVector val = E_loglik(theta, omega_hat);
    return -as<double>(val);
  }

  // Gradient: -E_loglik_grad(theta, omega_hat)
  NumericVector gradient(NumericVector theta) {
    if (E_loglik_grad.isNULL())
      return NumericVector(); // handled at R level

    NumericVector val = E_loglik_grad(theta, omega_hat);
    return -val;
  }

  // Equality constraint: psi_fn(theta) - psi_target
  NumericVector heq(NumericVector theta) {
    NumericVector val = psi_fn(theta);
    return val - psi_target;
  }
};

RCPP_MODULE(PsiOptModule) {
  class_<PsiEvaluator>("PsiEvaluator")
  .constructor<Function, Function, Function>()
  .method("set_omega_hat", &PsiEvaluator::set_omega_hat)
  .method("set_psi_target", &PsiEvaluator::set_psi_target)
  .method("objective", &PsiEvaluator::objective)
  .method("gradient", &PsiEvaluator::gradient)
  .method("heq",        &PsiEvaluator::heq);
}
#include <Rcpp.h>
using namespace Rcpp;

// -------------------------------------------------------------------
// PsiEvaluator class: holds omega_hat, psi_target, and model functions
// -------------------------------------------------------------------

class PsiEvaluator {
public:

  // Stored data
  NumericVector omega_hat;
  NumericVector psi_target;

  // R functions passed from R
  Function E_loglik;
  Function E_loglik_grad;   // may be NULL
  Function psi_fn;

  // Constructor
  PsiEvaluator(Function E_loglik_,
               Function E_loglik_grad_,
               Function psi_fn_)
    : E_loglik(E_loglik_), E_loglik_grad(E_loglik_grad_), psi_fn(psi_fn_) {}

  // Set omega_hat from R
  void set_omega_hat(NumericVector om) {
    omega_hat = clone(om);
  }

  // Set psi_target from R
  void set_psi_target(NumericVector pt) {
    psi_target = clone(pt);
  }

  // -------------------------
  // Objective: -E_loglik(θ, ω̂)
  // -------------------------
  double objective(NumericVector theta) {
    NumericVector out = E_loglik(theta, omega_hat);
    return -as<double>(out);
  }

  // ------------------------------------------------
  // Gradient: -E_loglik_grad(θ, ω̂) or empty vector
  // ------------------------------------------------
  NumericVector gradient(NumericVector theta) {
    if (E_loglik_grad.isNULL()) {
      return NumericVector();   // auglag interprets as no gradient
    }
    NumericVector out = E_loglik_grad(theta, omega_hat);
    return -out;
  }

  // ----------------------------------------------------
  // Equality constraint: psi_fn(θ) - psi_target
  // ----------------------------------------------------
  NumericVector heq(NumericVector theta) {
    NumericVector out = psi_fn(theta);
    return out - psi_target;
  }
};

// ---------------------------------------------------------------
// Expose module to R
// ---------------------------------------------------------------
RCPP_MODULE(PsiEvaluatorModule) {
  class_<PsiEvaluator>("PsiEvaluator")
  .constructor<Function, Function, Function>()
  .method("set_omega_hat", &PsiEvaluator::set_omega_hat)
  .method("set_psi_target", &PsiEvaluator::set_psi_target)
  .method("objective", &PsiEvaluator::objective)
  .method("gradient",  &PsiEvaluator::gradient)
  .method("heq",       &PsiEvaluator::heq);
}
