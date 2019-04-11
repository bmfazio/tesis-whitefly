# Fit a Zero-Inflated Binomial model with constant inflation probability
fit_zib <- function(formula_, ...){
  priors <- set_prior("normal(0,5)", class = "b") # weak regularization
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = zero_inflated_binomial())
}

# Fit an Endpoint-Inflated Binomial model w/softmax mixture link
fit_eiBinomialSM <- function(formula_, ...){
  eiBinomialSM <- custom_family(
    "eiBinomialSM", dpars = c("mu", "so", "sm"), # unrestricted mixture scores
    links = c("logit", "identity", "identity"),
    type = "int", vars = "trials[n]"
  )
  
  stan_funs <- "
  vector linkfunc(real so, real sm){
  return softmax([so, sm, 0]');
  }
  
  int[] ei(int y, int ntrial) {
  return {(1 - min({1, y})), (1 - min({1, ntrial - y}))};
  }
  
  real eiBinomialSM_lpmf(int y, real mu, real so, real sm, int trials) {
  int yom[2] = ei(y, trials);
  vector[3] pom = linkfunc(so, sm);
  
  return log(yom[1]*pom[1] + yom[2]*pom[2] + pom[3]*exp(binomial_logit_lpmf(y | trials, mu)));
  }
  
  int eiBinomialSM_rng(real mu, real so, real sm, int trials) {
  int which_component = categorical_rng(linkfunc(so, sm));
  
  if (which_component == 1) {
  return 0;
  }
  if (which_component == 2) {
  return trials;
  }
  
  return binomial_rng(trials, inv_logit(mu));
  }
  "
  
  stanvars <- stanvar(scode = stan_funs, block = "functions") +
    stanvar(as.integer(wf$bindenom), name = "trials")
  
  # Reasonable weak regularization for logistic coeffs
  priors <- set_prior("normal(0,5)", class = "b")
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = eiBinomialSM,
      stanvars = stanvars,
      ...)
  }
