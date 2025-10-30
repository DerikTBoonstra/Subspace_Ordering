# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# Configuration -----------------------------------------------------------
p <- 50
s <- .6*p

set.seed(1)
beta <- cbind(
  c(runif(s, 0.3, 0.6), rep(0, p - s)),
  c(runif(s/2, 0.3, 0.6), runif(s/2, -.6, -.3), rep(0, p - s))
)

x_fn <- function(n){
  mu <- list(
    c(rep(-1, s), rep(0, p - s)),
    rep(0, p),
    c(rep(1, s), rep(0, p - s))
  )
  Sigma <- list(
    sparsediscrim::cov_autocorrelation(p, rho = 0.10),
    sparsediscrim::cov_autocorrelation(p, rho = 0.50),
    sparsediscrim::cov_autocorrelation(p, rho = 0.90)
  )
  
  idx <- sample(1:3, n, replace = TRUE, prob = c(.4, .2, .4))
  
  do.call(rbind, lapply(seq_len(n), function(i){
    MASS::mvrnorm(1, mu[[idx[i]]], Sigma[[idx[i]]])
  }))
}

y_fn <- function(x, beta){
  n <- nrow(x)
  x %*% as.matrix(beta[,1]) * exp(x %*% as.matrix(beta[,2]) + 0.5*rnorm(n))
}

# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = 7)
  config_D3 <- sim_wrapper_D(beta, x_fn, y_fn, n = c(p + 1, 2*p, 5*p), nsims = 1000)
  plan(sequential) 
  # save(config_D3,  file ="saved_results/config_D3.RData")
}

config_D3 |> 
  pivot_longer(-n, names_to = "method", values_to = "D") |> 
  mutate("method" = as_factor(method)) |> 
  ggplot(aes(method, D, fill = method))+
  geom_boxplot()+
  facet_wrap(~n)+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176")
  )+
  theme(strip.background =element_rect(fill="lightgrey"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")+
  ylim(0, 1)
