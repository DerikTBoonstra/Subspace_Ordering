# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")


# Read and Clean Data -----------------------------------------------------
{
  data(BreastCancer, package = "mlbench")
  dat <- BreastCancer |> 
    select(-Id) |> 
    janitor::clean_names() |> 
    mutate(across(-class, as.numeric), 
           "class" = case_when(class == "benign" ~ 1, 
                               class == "malignant"~ 2)) |> 
    drop_na() |> 
    relocate(class, .before = everything()) 
  rm(BreastCancer)
}


# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = parallel::detectCores() - 1)
  bc_out <- data_appl_wrapper(data = dat, nsims = 1000, dim.order = "t")
  plan(sequential)
  # save(bc_out, file = "saved_results/bc_app_out.RData")
}

load(file = "saved_results/bc_app_out.RData")
bc_out |> 
  data_app_summary()

