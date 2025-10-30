# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal())


# Load Data ---------------------------------------------------------------
{
  dat <- read_csv(file = "datasets/TCGA_InfoWithGrade.csv") |> 
    rename("class" = Grade) |>
    mutate(across(where(is.character), function(x) as.numeric(as.factor(x))),
           "class" = as_factor(class))
}
# Dim Ordering -------------------------------------------------------------------------
nsims <- 1000
T_boot_out <- parallel::mclapply(1:nsims, function(sim){
  set.seed(sim)

  p <- ncol(dat) - 1
  n <- nrow(dat)
  boot <- dat |> 
    slice_sample(n = n, replace = TRUE) |>
    mutate(
      across(-c(class, Age_at_diagnosis), function(x) x + rnorm(n(), sd = 0.000001))
    )
    
  sdr_t_out <- sdr::sdr(boot[,-1], boot$class, method = "sdrs", prec.est = NULL, dim.order = "t2")

  Tj <- vector(length = p)
  Tj[sdr_t_out$dims] <- sdr_t_out$dim_criteria
  
  ## Return
  data.frame("Tj" = Tj, "dim" = 1:p)

}, mc.cores = parallel::detectCores() - 1) |> 
  do.call(rbind, args = _)
# save(T_boot_out, file = "T_boot_out.RData")

load(file = "saved_results/T_boot_out.RData")
T_boot_out |> 
  ggplot(aes(as_factor(dim), Tj))+
    geom_boxplot(outlier.alpha = .2)+
  labs(
       x = "SSDR Dimenion Reduction Subspace",
       y = expression("T"["j"]))

Ts <- (T_boot_out |> 
  group_by(as_factor(dim)) |> 
  summarize("mean" = median(Tj, na.rm = TRUE)) |> 
  pull(mean) |> 
  sort(decreasing = TRUE, index.return = TRUE))$ix

error_boot_out <- parallel::mclapply(1:nsims, function(sim){
  set.seed(2025 + sim)
  
  p <- ncol(dat) - 1
  n <- nrow(dat)
  boot <- dat |> 
    slice_sample(n = n, replace = TRUE) |>
    mutate(
      across(-c(class, Age_at_diagnosis), function(x) x + rnorm(n(), sd = 0.000001))
    )
  
  sdr_out <- sdr::sdr(boot[,-1], boot$class, method = "sdrs", prec.est = NULL)
  sdr_t_out <- sdr::sdr(boot[,-1], boot$class, method = "sdrs", prec.est = NULL, dims = Ts)
  
  lapply(list(sdr_out, sdr_t_out), function(x){
    lapply(1:p, function(dhat){
      mean(boot$class != predict(x, model = "qda", ndims = dhat)$class)
    }) |>
      do.call(rbind, args = _)
  }) |>
    do.call(rbind, args = _) |>
    data.frame("error" = _) |>
    mutate("method" = rep(c("SDRS", "SDRS_T"), each = n()/2),
           "dim" = rep(1:(n()/2), times = 2))
}, mc.cores = parallel::detectCores() - 1) |> 
  do.call(rbind, args = _)
# save(error_boot_out, file = "error_boot_out.RData")

load(file = "saved_results/error_boot_out.RData")
error_boot_out |> 
  group_by(method) |> 
  group_split() |> 
  lapply(function(x){
   x |> 
      group_by(dim) |> 
      summarise("error" = median(error)) |> 
      pull(error)
  })

qda_med <- error_boot_out |> 
  filter(dim == 23) |> 
  pull(error) |> 
  median()

error_boot_out |> 
  filter(dim != 23) |> 
  ggplot(aes(as_factor(dim), error, fill = method))+
  geom_boxplot(outlier.alpha = .15, outlier.size = .25)+
  geom_hline(aes(yintercept = qda_med, color = "QDA"), data = NULL)+
  scale_fill_manual(values = c("purple", "#fde725"), 
                    labels = c("SSDR", expression("SSDR"["T"])))+
  scale_color_manual(values = "red")+
  theme(legend.position = "top")+
  labs(x = expression(widehat(d)), y = expression(widehat("AER")), 
       fill = "", color = "")
