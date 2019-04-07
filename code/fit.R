fit_zib <- function(formula_){
  # Reasonable weak regularization for logistic coeffs
  priors <- set_prior("normal(0,5)", class = "b")
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = zero_inflated_binomial())
}

# Modelo 1: Hall 2004
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# logit(mix_ijk) = c
m1 <- fit_zib(nlive|trials(bindenom) ~ trt + week + rep)




eib_fit <- brm(
  nlive ~ trt + week + (1|plantid) + (1|rep), data = wf,
  family = ei_binomial, stanvars = stanvars, prior = priors,
  cores = 3, control = list(adapt_delta = 0.95),
  inits = list(a, a, a, a)
)

# inits?
#a <- list(b = as.array(rep(0, 6)), po = 0.33, pm = 0.33)