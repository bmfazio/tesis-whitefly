set.seed(69420)

# Model 1a: Hall 2004 ZIB
save_fit("zib_fixed", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep)

# Model 1b: Hall 2004 + random intercept
save_fit("zib_randeff", fit_zib,
         nlive|trials(bindenom) ~ trt + week + rep + (1|plantid))

# Model 1c: Hall 2004 + trt on 0-inflation
save_fit("zib_0trt", fit_zib,
         bf(nlive|trials(bindenom) ~ trt + week + rep,
            zi ~ trt))

# Model 1d: Hall 2004 + predict 0-inflation
save_fit("zib_0pred", fit_zib,
         bf(nlive|trials(bindenom) ~ trt + week + rep,
            zi ~ trt + week + rep))

# Model 2a: Hall 2004 EIB (fixed inflation)
save_fit("eib_fixed", fit_eiBinomialSM,
         nlive ~ trt + week + rep)

# Model 2b: Hall 2004 + trt on 0-inflation
save_fit("eib_0trt", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt))

# Model 2c: Hall 2004 + trt on 0-inflation
save_fit("eib_0pred", fit_eiBinomialSM,
         bf(nlive ~ trt + week + rep,
            so ~ trt + week + rep))

# Model 3a: Hall 2004 EIB (fixed inflation)
save_fit("neib_fixed", fit_eiBinomialLN,
         nlive ~ trt + week + rep)

# Model 3b: Hall 2004 + trt on 0-inflation
save_fit("neib_0trt", fit_eiBinomialLN,
         bf(nlive ~ trt + week + rep,
            muL ~ trt),
         control = list(max_treedepth = 12))

# Model 3c: Hall 2004 + trt on 0-inflation
save_fit("neib_0pred", fit_eiBinomialLN,
         bf(nlive ~ trt + week + rep,
            muL ~ trt + week + rep))

# # Model 3d: Hall 2004 + trt on 0-inflation
# save_fit("neib_semivaso", fit_eiBinomialLN,
#          bf(nlive ~ trt + week + rep,
#             muL ~ trt,
#             sdL ~ trt),
#          control = list(adapt_delta = 0.95, max_treedepth = 12))
# 
# save_fit("neib_fullvaso", fit_eiBinomialLN,
#          bf(nlive ~ trt + week + rep,
#             muL ~ trt,
#             sdL ~ trt + week + rep),
#          control = list(adapt_delta = 0.9, max_treedepth = 12))


m1 <- readRDS("out/fits/zib_fixed.rds")
m2 <- readRDS("out/fits/zib_randeff.rds")
m3 <- readRDS("out/fits/zib_0trt.rds")
m4 <- readRDS("out/fits/zib_0pred.rds")
m5 <- readRDS("out/fits/eib_fixed.rds")
m6 <- readRDS("out/fits/eib_0trt.rds")
m7 <- readRDS("out/fits/eib_0pred.rds")
m8 <- readRDS("out/fits/neib_fixed.rds")
m9 <- readRDS("out/fits/neib_0trt.rds")
mA <- readRDS("out/fits/neib_0pred.rds")
mB <- readRDS("out/fits/neib_fullvaso.rds")

expose_functions(m5, vectorize = TRUE)
lm5 <- loo(m5)
lm6 <- loo(m6)
lm7 <- loo(m7)
expose_functions(m8, vectorize = TRUE)
lm8 <- loo(m8)
lm9 <- loo(m9)
lmA <- loo(mA)

# marginal_effects(m4)
# marginal_effects(m5)
# marginal_effects(m6)
# loo(m4, m5, m6)