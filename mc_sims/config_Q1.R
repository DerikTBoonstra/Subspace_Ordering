# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# configuration ----------------------------------------------------------
p <- 50
set.seed(1)
mu <- list(
  rep(0, p),
  rnorm(p)
)

Sigma1 <- diag(p)
Sigma2 <- Sigma1
Sigma2[1:2, 1:2] <- matrix(c(3, -2, -2, 3), 2, 2)

Sigma <- list(Sigma1, Sigma2)


(d <- qr(cbind(solve(Sigma[[1]]) %*% mu[[1]] - solve(Sigma[[2]]) %*% mu[[2]], 
              solve(Sigma[[1]]) - solve(Sigma[[2]])))$rank)
# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_Q1_out <- sim_wrapper(d, mu, Sigma, n = 5000, ni = c(p + 1, 2*p, 5*p), nsims = 1000, dim.order = "t2", model = "qda")
  plan(sequential)
  # save(config_Q1_out, file = "saved_results/config_Q1_out.RData")
}

config_Q1_out |>
  sim_summary()

config_Q1_out |>
  sim_boxplots(model = "qda")
