save_fit <- function(fitname, fitfun, formula_) {
  if(file.exists("out/fits/" %p0% fitname %p0% ".rds"))stop("File already exists")
  saveRDS(object = fitfun(formula_), file = "out/fits/" %p0% fitname %p0% ".rds")
}

fit_zib <- function(formula_){
  # Reasonable weak regularization for logistic coeffs
  priors <- set_prior("normal(0,5)", class = "b")
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = zero_inflated_binomial())
}

fit_eib <- function(formula_){
  ei_binomial <- custom_family(
    "ei_binomial", dpars = c("mu", "s0", "s1"),
    links = c("logit", "identity", "identity"),
    type = "int", vars = "trials[n]"
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
  
  # Reasonable weak regularization for logistic coeffs
  priors <- set_prior("normal(0,5)", class = "b")
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = zero_inflated_binomial())
}


# Model 1: Hall 2004
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# logit(mix_ijk) = c
save_fit("hall_fixedeff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep)

# Model 2: Hall 2004 + intra-unit correlation via random intercept
# logit(p_ijk) = mu + block_j + trt_i + b*week_k + (1|plantid)
# logit(mix_ijk) = c
save_fit("hall_idraneff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep + (1|plantid))

# Model 2: Hall 2004 + intra-unit correlation via random intercept
# logit(p_ijk) = mu + block_j + trt_i + b*week_k + (1|plantid)
# logit(mix_ijk) = c
save_fit("hall_idraneff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep + (1|plantid))

# Model 3: Hall 2004 + reff + endpoint inflation
# logit(p_ijk) = mu + block_j + trt_i + b*week_k + (1|plantid)
# logit(mix0_ijk) = c
# logit(mix1_ijk) = c
save_fit("hall_idraneff", fit_eib,
         nlive|trials(bindenom) ~ trt + week + rep + (1|plantid))



###
m1 <- readRDS("out/fits/hall_fixedeff.rds")
m2 <- readRDS("out/fits/hall_idraneff.rds")

marginal_effects(m1)
marginal_effects(m2)

eib_fit <- brm(
  nlive ~ trt + week + (1|plantid) + (1|rep), data = wf,
  family = ei_binomial, stanvars = stanvars, prior = priors,
  cores = 3, control = list(adapt_delta = 0.95),
  inits = list(a, a, a, a)
)

# inits?
#a <- list(b = as.array(rep(0, 6)), po = 0.33, pm = 0.33)