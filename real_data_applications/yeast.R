# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")


# Read and Clean Data -----------------------------------------------------
{
  dat <- read_table("datasets/yeast.data", col_names = FALSE) |> 
    select(-1) |> 
    rename("class" = last_col()) |> 
    relocate(class, .before = everything()) |> 
    filter(class != "ERL") 
  set.seed(1)
  dat[,6:7] <- dat[,6:7] + rnorm(nrow(dat)*ncol(dat[,6:7]), sd = 0.00001)
}


# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = parallel::detectCores() - 1)
  yeast_out <- data_appl_wrapper(data = dat, nsims = 1000, dim.order = "F")
  plan(sequential)
  # save(yeast_out, file = "saved_results/yeast_app_out.RData")
}

load(file = "saved_results/yeast_app_out.RData")
yeast_out |> 
  data_app_summary()


