# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# Configuration -----------------------------------------------------------
p <- 50
mu <- list(rep(0, p), 
           rep(1, p))

Sigma <- list(.75*diag(p) + .25, .75*diag(p) + .25)

# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_L1_out <- sim_wrapper(d = 1, mu, Sigma, n = 5000, ni = c(p + 1, 2*p, 5*p), nsims = 1000, dim.order = "t2", model = "lda")
  plan(sequential)
  # save(config_L1_out, file = "saved_results/config_L1_out.RData")
}

config_L1_out |>
  sim_summary()
config_L1_out |>
  sim_boxplots()
