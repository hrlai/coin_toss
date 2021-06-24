library(greta)
library(bayesplot)

# inverse logit function
inv_logit <- function(x) {
    exp(x) / (exp(x) + 1)
}

# 100 coin toss without success
y <- rep(0, 100)

# fit lm model
mod_lm <- glm(y ~ 1, family = stats::binomial())
summary(mod_lm)
inv_logit(coef(mod_lm))

# fit greta model
int <- normal(0, 100)
p <- ilogit(int)
distribution(y) <- bernoulli(p)
mod_greta <- model(p, int)
draws <- mcmc(mod_greta)
summary(draws)
mcmc_trace(draws)
