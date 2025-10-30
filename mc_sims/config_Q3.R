# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# configuration ----------------------------------------------------------
p <- 50
mu <- list(rep(0, p),
           c(rep(0, p - 1), 1))

Sigma <- list({
  Sigma1 <- diag(c(rep(2, p - 1), 1))
  Sigma1
}, Sigma1)

(d <- qr(cbind(solve(Sigma[[1]]) %*% mu[[1]] - solve(Sigma[[2]]) %*% mu[[2]], 
         solve(Sigma[[1]]) - solve(Sigma[[2]])))$rank)


# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_Q3_out <- sim_wrapper(d, mu, Sigma, n = 5000, ni = c(p + 1, 2*p, 5*p), nsims = 1000, dim.order = "t2", model = "qda")
  plan(sequential)
  # save(config_Q3_out, file = "saved_results/config_Q3_out.RData")
}

config_Q3_out |>
  sim_summary()
config_Q3_out |>
  sim_boxplots(model = "qda")
