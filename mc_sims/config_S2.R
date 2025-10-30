# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# Configuration -----------------------------------------------------------
p <- 50 
mu1 <- c(1, 1, rep(0, p-2))
mu2 <- -mu1
mu <- list(mu1, mu2, rep(0, p))
rho <- 0.99
s2 <- 10
Sigma_signal <- diag(2)
Sigma_nuis <- s2 * ((1-rho)*diag(p-2) + rho*matrix(1, p-2, p-2))
S <- as.matrix(Matrix::bdiag(Sigma_signal, Sigma_nuis))
Sigma <- list(S, S, S)

(d <- qr(cbind(solve(Sigma_mat) %*% (mu[[3]] - mu[[1]]), solve(Sigma_mat) %*% (mu[[2]] - mu[[1]]) ))$rank)
# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_S2_out <- sim_wrapper(d = d, mu, Sigma, n = 5000, ni = c(p + 1, 2*p, 5*p), nsims = 1000, dim.order = "F", model = "lda")
  plan(sequential)
  # save(config_S2_out, file = "saved_results/config_S2_out.RData")
}

config_S2_out |>
  sim_summary()

config_S2_out |>
  sim_boxplots()
