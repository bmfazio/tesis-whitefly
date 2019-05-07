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
    links = c("identity", "identity", "identity"),
    type = "int", vars = "trials[n]",
    log_lik =
      function(i, draws) {
        mu <- if(is.null(dim(draws$dpars$mu))) {
          draws$dpars$mu
        } else{
          draws$dpars$mu[, i]
        }
        so <- if(is.null(dim(draws$dpars$so))) {
          draws$dpars$so
        } else{
          draws$dpars$so[, i]
        }
        sm <- if(is.null(dim(draws$dpars$sm))) {
          draws$dpars$sm
        } else{
          draws$dpars$sm[, i]
        }
        trials <- draws$data$trials[i]
        y <- draws$data$Y[i]
        eiBinomialSM_lpmf(y, mu, so, sm, trials)
      },
    predict =
      function(i, draws, ...) {
        mu <- if(is.null(dim(draws$dpars$mu))) {
          draws$dpars$mu
        } else{
          draws$dpars$mu[, i]
        }
        so <- if(is.null(dim(draws$dpars$so))) {
          draws$dpars$so
        } else{
          draws$dpars$so[, i]
        }
        sm <- if(is.null(dim(draws$dpars$sm))) {
          draws$dpars$sm
        } else{
          draws$dpars$sm[, i]
        }
        trials <- draws$data$trials[i]
        eiBinomialSM_rng(mu, so, sm, trials)
      },
    fitted =
      function(draws) {
        mu <- draws$dpars$mu
        so <- draws$dpars$so
        sm <- draws$dpars$sm
        p <- split_nrow(mapply(softmax2, so, sm), 3)[-1]
        trials <- median(draws$data$trials)
        
        (p[[1]] + p[[2]]*inv_logit_scaled(mu))*trials
      }
    
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

# Fit an Endpoint-Inflated Binomial model w/latent normal link
fit_eiBinomialLN <- function(formula_, ...){
  eiBinomialLN <- custom_family(
    "eiBinomialLN", dpars = c("mu", "muL", "sdL"), # unrestricted mixture scores
    links = c("identity", "identity", "identity"),
    type = "int", vars = "trials[n]",
    log_lik =
      function(i, draws) {
        mu <- if(is.null(dim(draws$dpars$mu))) {
          draws$dpars$mu
        } else{
          draws$dpars$mu[, i]
        }
        muL <- if(is.null(dim(draws$dpars$muL))) {
          draws$dpars$muL
        } else{
          draws$dpars$muL[, i]
        }
        sdL <- if(is.null(dim(draws$dpars$sdL))) {
          draws$dpars$sdL
        } else{
          draws$dpars$sdL[, i]
        }
        trials <- draws$data$trials[i]
        y <- draws$data$Y[i]
        eiBinomialLN_lpmf(y, mu, muL, sdL, trials)
      },
    predict =
      function(i, draws, ...) {
        mu <- if(is.null(dim(draws$dpars$mu))) {
          draws$dpars$mu
        } else{
          draws$dpars$mu[, i]
        }
        muL <- if(is.null(dim(draws$dpars$muL))) {
          draws$dpars$muL
        } else{
          draws$dpars$muL[, i]
        }
        sdL <- if(is.null(dim(draws$dpars$sdL))) {
          draws$dpars$sdL
        } else{
          draws$dpars$sdL[, i]
        }
        trials <- draws$data$trials[i]
        eiBinomialLN_rng(mu, muL, sdL, trials)
      },
    fitted =
      function(draws) {
        mu <- draws$dpars$mu
        muL <- draws$dpars$muL
        sdL <- draws$dpars$sdL
        p <- split_nrow(mapply(cutnorm2, muL, exp(sdL)), 3)[-1]
        trials <- median(draws$data$trials)
        
        (p[[1]] + p[[2]]*inv_logit_scaled(mu))*trials
      }
    
  )
  
  stan_funs <- "
  vector linkfunc(real muL, real sdL){
  real lsdL = exp(sdL);
  real p1 = normal_cdf(-1, muL, lsdL);
  real p2 = normal_cdf(1, muL, lsdL);
    
    return [p1, 1-p2, p2-p1]';
  }
  
  int[] ei(int y, int ntrial) {
  return {(1 - min({1, y})), (1 - min({1, ntrial - y}))};
  }
  
  real eiBinomialLN_lpmf(int y, real mu, real muL, real sdL, int trials) {
  int yom[2] = ei(y, trials);
  vector[3] pom = linkfunc(muL, sdL);
  
  return log(yom[1]*pom[1] + yom[2]*pom[2] + pom[3]*exp(binomial_logit_lpmf(y | trials, mu)));
  }
  
  int eiBinomialLN_rng(real mu, real muL, real sdL, int trials) {
  int which_component = categorical_rng(linkfunc(muL, sdL));
  
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
  priors <- set_prior("student_t(3, 0, 10)", class = "b")
  brm(formula = formula_,
      data = wf,
      prior = priors,
      family = eiBinomialLN,
      stanvars = stanvars,
      ...)
  }
