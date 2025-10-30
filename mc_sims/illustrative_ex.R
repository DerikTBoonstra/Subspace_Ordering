library("tidyverse");theme_set(theme_minimal());library("patchwork")

# Configuration  -------------------------------------------------------------------
Sigma <- diag(c(3, 2, 1))
p <- ncol(Sigma)
alpha <- 5
epsilon <- 2
mu <- list(
  rep(0, p), 
  c(0, epsilon, alpha)
)


# Delta and V -------------------------------------------------------------
mu_d <- Reduce(`-`, mu)
V <- eigen(Sigma)$vectors
(Deltas <- sapply(1:ncol(V), function(i){
  v <- as.matrix(V[,i])
  abs(t(v) %*% mu_d)/sqrt(t(v) %*% Sigma %*% v*.5 + t(v) %*% Sigma %*% v*.5)
}))


# Simulation --------------------------------------------------------------
set.seed(1)
n <- 1000
X <- lapply(seq_along(mu), function(i){
  MASS::mvrnorm(n = n, mu[[i]], Sigma) |> 
    as.data.frame() |> 
    mutate("class" = rep(i, n()))
}) |> 
  do.call(rbind, args = _)



# Plots -------------------------------------------------------------------
## TRUE V known
proj_dat <- lapply(1:ncol(V), function(i){
  as.matrix(X[,-4]) %*% as.matrix(V[,i])
}) |> 
  do.call(cbind, args = _) |> 
  as.data.frame() |> 
  mutate("class" = as_factor(X$class))

lapply(1:3, function(i){
  x_lab <- bquote(bold(v)[.(i)] ~ "subspace")
  
  set.seed(1)
  p <- tibble("x" = proj_dat[, i], 
              "Class" = proj_dat$class) |>
    ggplot(aes(x, 0, color = Class, shape = Class)) +
    geom_jitter(alpha = .75) +
    scale_color_manual(values = c("black", "#fea973")) +
    labs(x = x_lab, y = "Jittered")+
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  if (i != 1) {
    p <- p + theme(
      axis.title.y = element_blank()
    )
  }
  p
}) |> 
  wrap_plots()+
  plot_layout(guides = "collect") & 
  theme(legend.position = "top")


# Errors ------------------------------------------------------------------
(sdr_out <- sdr::sdr(X[,-4], X$class, method = "pca"))
sdr_out$eigvalues

sdr::dim_order(object = sdr_out, method = "t2")
lapply(1:3, function(i){
  mean(predict(MASS::lda(as.matrix(sdr_out$ProjectedData[,1:i]), sdr_out$slices))$class != X$class)
})
