library(greta)
library(bayesplot)
library(brglm2)

# inverse logit function
inv_logit <- function(x) {
    exp(x) / (exp(x) + 1)
}

# 78 coin toss without success
y <- rep(0, 78)

# fit lm model / note SE of coefficient
mod_lm <- glm(y ~ 1, family = stats::binomial())
summary(mod_lm)
inv_logit(coef(mod_lm))

# fit lm model with Firth/bias-reduced logistic regression / note SE of coefficient
mod_lm_firth <- glm(y ~ 1, family = stats::binomial(), method=brglmFit)
summary(mod_lm_firth)
inv_logit(coef(mod_lm_firth))

# fit greta model
int <- normal(0, 100)
p <- ilogit(int)
distribution(y) <- bernoulli(p)
mod_greta <- model(p, int)
draws <- mcmc(mod_greta)
summary(draws)
mcmc_trace(draws)
