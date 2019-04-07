ei_binomial <- custom_family(
  "ei_binomial",
  dpars = c("mu", "po", "pm"),
  links = rep("identity", 3),
  lb = c(NA, 0, 0), ub = c(NA, 1, 1),
  type = "int", vars = c("yo[n]", "ym[n]", "trials[n]")
)

stan_funs <- "
real ei_binomial_lpmf(int y, real mu, real po, real pm, int yo, int ym, int T) {
  return log(yo*po + ym*pm + (1-po-pm)*exp(binomial_logit_lpmf(y|T, mu)));
}

int ei_binomial_rng(real mu, real po, real pm, int T) {
  int which_component = categorical_rng([po, pm, (1-po-pm)]');

  if (which_component == 1) {
    return 0;
  }
  if (which_component == 2) {
    return T;
  }

  return binomial_rng(T, inv_logit(mu));
}
"

stanvars <- stanvar(scode = stan_funs, block = "functions") +
  stanvar(as.integer(wf$bindenom), name = "trials") +
  stanvar(as.integer(wf$nlive == 0), name = "yo") +
  stanvar(as.integer(wf$nlive == wf$bindenom), name = "ym")
