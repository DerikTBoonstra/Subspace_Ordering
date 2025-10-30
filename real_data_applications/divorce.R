# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())
library("future.apply");library("progressr");handlers(global = TRUE);handlers("progress")
source("wrapper_fns.R")


# Read and Clean Data -----------------------------------------------------
dat <- read_delim("datasets/divorce.csv", delim = ";") |> 
  janitor::clean_names() |> 
  relocate(class, .before = everything()) |>
  mutate(across(where(is.character), as_factor),
         across(-class, as.numeric),
         "class" = case_when(class  == "0" ~ 1,
                             class == "1" ~ 2) |>
           as_factor()
  )

## adding error for singularity issues
set.seed(1)
dat[,-1] <- dat[,-1] + matrix(rnorm(nrow(dat[,-1])*ncol(dat[,-1]), sd = .00001), nrow = nrow(dat))


# Simulation --------------------------------------------------------------
{
  plan(multisession, workers = parallel::detectCores() - 1)
  divorce_out <- data_appl_wrapper(data = dat, nsims = 1000, dim.order = "t")
  plan(sequential)
  # save(divorce_out, file = "saved_results/divorce_app_out.RData")
}

load(file = "saved_results/divorce_app_out.RData")
divorce_out |> 
  data_app_summary()
