# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# configuration ----------------------------------------------------------
p <- 50
mu <- list(rep(0, times = p), #mu0
           c(rep(c(1, -1), each = 5), rep(0, times = p - 10)))#mu1

block_rv <- function(p, b, rho){
  ul <- rho*diag(b)
  ul[row(ul) != col(ul)] <- abs(1 - rho)
  u <- cbind(ul, matrix(0, ncol = p - b, nrow = b))
  l <- cbind(matrix(0, ncol = b, nrow = p - b), diag(p - b))
  rbind(u, l)
}

Sigma <- list(diag(p),
              block_rv(p, 5, rho = .99))

(d <- qr(cbind(solve(Sigma[[1]]) %*% mu[[1]] - solve(Sigma[[2]]) %*% mu[[2]], 
               solve(Sigma[[1]]) - solve(Sigma[[2]])))$rank)
# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_Q2_out <- sim_wrapper(d, mu, Sigma, n = 5000, ni = c(p + 1, 2*p, 5*p), nsims = 1000, dim.order = "t2", model = "qda")
  plan(sequential)
  # save(config_Q2_out, file = "saved_results/config_Q2_out.RData")
}

config_Q2_out |>
  sim_summary()

config_Q2_out |>
  sim_boxplots(model = "qda")
