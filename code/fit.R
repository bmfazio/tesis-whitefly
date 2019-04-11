# Model 1a: Hall 2004
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# logit(mix_ijk) = c
save_fit("hall_fixedeff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep)

# Model 1b: Hall 2004 + random intercept
# logit(p_ijk) = mu + block_j + trt_i + b*week_k + (1|plantid)
# logit(mix_ijk) = c
save_fit("hall_idraneff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep + (1|plantid))

# Model 1c: Hall 2004 + predictor on zero probability
save_fit("hall_zpred", fit_zib,
         bf(nlive|trials(bindenom) ~ trt + week + rep,
            zi ~ trt + week + rep))

# Model 2a: Hall 2004 - EIB (fixed EI)
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# logit(mix0_ijk) = c
# logit(mix1_ijk) = c
save_fit("hall_eibPconst", fit_eiBinomialSM,
         nlive ~ trt + week + rep)

# Model 2b: Hall 2004 - EIB (same predictor for all parameters)
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# [mix0_ijk, mix1_ijk] = softmax(mu + block_j + trt_i + b*week_k)
save_fit("hall_eibfullreg", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt + week + rep,
            sm ~ trt + week + rep))
  # Does NOT converge as-is
  # Got max_treedepth warnings on every transition

# Model 2c: Hall 2004 - EIB (only main var on inflation)
# logit(p_ijk) = mu + block_j + trt_i + b*week_k
# [mix0_ijk, mix1_ijk] = softmax(trt_i)
save_fit("hall_eibtrtreg2", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt,
            sm ~ trt),
         control = list(adapt_delta = 0.9, max_treedepth = 15))
save_fit("hall_eibtrtreg", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt,
            sm ~ trt))

###
m1 <- readRDS("out/fits/hall_fixedeff.rds")
m2 <- readRDS("out/fits/hall_idraneff.rds")
m3 <- readRDS("out/fits/hall_eibPconst.rds")

### ARREGLA!!!!!!!!1111111111111
expose_functions(m3, vectorize = TRUE)
softmax <- function(x)(exp(x)/sum(exp(x)))
log_lik_eiBinomialSM <- function(i, draws) {
  mu <- draws$dpars$mu[, i]
  so <- draws$dpars$so
  sm <- draws$dpars$sm
  trials <- draws$data$trials[i]
  y <- draws$data$Y[i]
  eiBinomialSM_lpmf(y, mu, so, sm, trials)
}
predict_eiBinomialSM <- function(i, draws, ...) {
  mu <- draws$dpars$mu[, i]
  so <- draws$dpars$so
  sm <- draws$dpars$sm
  trials <- draws$data$trials[i]
  eiBinomialSM_rng(mu, so, sm, trials)
}
fitted_eiBinomialSM <- function(draws) {
  mu <- draws$dpars$mu
  so <- draws$dpars$so
  sm <- draws$dpars$sm
  p <- t(apply(cbind(matrix(c(so, sm), ncol = 2), 0), 1, softmax))
  (p[,2] + p[,3]*mu)
}
draws1 <- readRDS("averps40.rds")
draws2 <- readRDS("averps66.rds")
draws3 <- readRDS("averps76.rds")
#marginal_effects(m3)
#pp_check(m3)
#loo(m3)
predict_eiBinomialSM <- function(i, draws, ...){1}

softmax(c(0.37, 0, -1.22))

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