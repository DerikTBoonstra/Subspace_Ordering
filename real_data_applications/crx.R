# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")

# Read and Clean Data -----------------------------------------------------
set.seed(1)
dat <- read_csv("datasets/crx.data", col_names = FALSE, na = "?") |>
  janitor::clean_names() |> 
  mutate("x14" = as.numeric(x14)) |> 
  rename("class" = "x1") |> 
  mutate(across(where(is.character), as_factor),
         across(-class, as.numeric), 
         "class" = case_when(class == "b" ~ 1, 
                             class == "a" ~ 2) |>
           as_factor()
  ) |> 
  drop_na()
dat[,-1] <- dat[,-1] + rnorm(nrow(dat)*ncol(dat[,-1]), sd = 0.0001)

# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = parallel::detectCores() - 1)
  crx_out <- data_appl_wrapper(data = dat, nsims = 1000, dim.order = "t")
  plan(sequential)
  # save(crx_out, file = "saved_results/crx_app_out.RData")
}

load(file = "saved_results/crx_app_out.RData")
crx_out |> 
  data_app_summary()
