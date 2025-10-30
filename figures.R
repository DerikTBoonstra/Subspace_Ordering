library("tidyverse");theme_set(theme_minimal());library("patchwork")

# Section 5.1 Classification Sims -----------------------------------------------------
for(i in 1:3){
  load(paste0("saved_results/config_Q", i, "_out.RData"))
}
for(i in 1:3){
  load(paste0("saved_results/config_L", i, "_out.RData"))
}
LDA_sims <- rbind(config_L1_out, 
                  config_L2_out, 
                  config_L3_out) |> 
  mutate("config" = rep(
    c("Configuration L1", "Configuration L2", "Configuration L3"), 
    each = n()/3), 
    ni = case_when(ni == 51 ~ "p + 1", 
                   ni == 100 ~ "2p", 
                   ni == 250 ~ "5p") |> 
      as_factor(), 
    method = as_factor(method)
  )

QDA_sims <- rbind(config_Q1_out, 
      config_Q2_out, 
      config_Q3_out) |> 
  mutate("config" = rep(
    c("Configuration Q1", "Configuration Q2", "Configuration Q3"), 
    each = n()/3), 
    ni = case_when(ni == 51 ~ "p + 1", 
                   ni == 100 ~ "2p", 
                   ni == 250 ~ "5p") |> 
      as_factor(), 
    method = as_factor(method)
    )

lda_meds <- LDA_sims |>
  filter(method == "LDA") |>
  group_by(ni, config) |> 
  summarise("med" = median(CER)) |> 
  ungroup()

qda_meds <- QDA_sims |>
  filter(method == "QDA") |>
  group_by(ni, config) |> 
  summarise("med" = median(CER)) |> 
  ungroup()

shade_df <- tribble(
  ~xmin, ~xmax, ~fill,
  0.5,   2.5,   "grey",   # PCA & PCA_T
  2.5,   4.5,   "white",   # SAVE & SAVE_T
  4.5,   6.5,   "grey",   # SIR-II & SIR_T
  6.5,   8.5,   "white"    # SSDR & SSDR_T
)

shade_data_LDA <- expand.grid(
  config = c("Configuration L1", "Configuration L2", "Configuration L3"),
  ni = factor(c("p + 1", "2p", "5p"), levels = c("p + 1", "2p", "5p"))
) |> 
  crossing(shade_df)

shade_data_QDA <- expand.grid(
  config = c("Configuration Q1", "Configuration Q2", "Configuration Q3"),
  ni = factor(c("p + 1", "2p", "5p"), levels = c("p + 1", "2p", "5p"))
) |> 
  crossing(shade_df)

LDA_sims |> 
  filter(method != "LDA") |> 
  ggplot(aes(method, CER, fill = method))+
  geom_rect(
    data = shade_data_LDA,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = NULL),
    inherit.aes = FALSE,
    fill = shade_data_LDA$fill,
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = .25, outlier.alpha = .25, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = "1"), data = lda_meds)+
  facet_grid(
    cols = vars(ni), 
    rows = vars(config), 
    scales = "free_y",
    labeller = label_bquote(cols = n == .(as.character(ni)))
  )+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176",
                               "lightgrey", "white"
  ))+
  scale_color_manual(values = "blue", labels = "LDA")+
  scale_x_discrete(labels = c(
    "PCA", expression("PCA"["T"]), 
    "SAVE", expression("SAVE"["T"]), 
    "SIR-II", expression("SIR-II"["T"]), 
    "SSDR", expression("SSDR"["T"])
  ))+
  scale_y_continuous(limits = c(0.05, .61), breaks = seq(.1, .6, by = .1))+
  guides(fill = "none")+
  theme(
    strip.background = element_rect(fill = "lightgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(), 
    legend.position = "bottom"
  )+
  labs(y = expression(widehat(CER)), color = "")

QDA_sims |> 
  filter(method != "QDA") |> 
  ggplot(aes(method, CER, fill = method))+
  geom_rect(
    data = shade_data_QDA,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = NULL),
    inherit.aes = FALSE,
    fill = shade_data_QDA$fill,
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = .25, outlier.alpha = .25, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = "1"), data = qda_meds)+
  facet_grid(
    cols = vars(ni), 
    rows = vars(config), 
    scales = "free_y",
    labeller = label_bquote(cols = n[h] == .(as.character(ni)))
  )+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176",
                               "lightgrey", "white"
  ))+
  scale_color_manual(values = "red", labels = "QDA")+
  scale_x_discrete(labels = c(
    "PCA", expression("PCA"["T"]), 
    "SAVE", expression("SAVE"["T"]), 
    "SIR-II", expression("SIR-II"["T"]), 
    "SSDR", expression("SSDR"["T"])
  ))+
  ggh4x::facetted_pos_scales(y = list(
    config ==  "Configuration Q1" ~ scale_y_continuous(limits = c(0, 0.55),
                                                       breaks = seq(0, .5, by = .1)),
    config ==  "Configuration Q2" ~ scale_y_continuous(limits = c(0, 0.55),
                                                       breaks = seq(0, .5, by = .1)),
    config ==  "Configuration Q3" ~ scale_y_continuous(limits = c(0.31, 0.56),
                                                       breaks = seq(.30, .55, by = .05))
  ))+
  guides(fill = "none")+
    theme(
      strip.background = element_rect(fill = "lightgrey"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(), 
      legend.position = "bottom"
    )+
  labs(y = expression(widehat(CER)), color = "")

# Section 5.2 Continuous Sims ---------------------------------------------------------
for(i in 1:3){
  load(paste0("saved_results/config_D", i, "_out.RData"))
}

sims <- rbind(config_D1, 
                  config_D2, 
                  config_D3) |> 
  mutate("config" = rep(
    c("Configuration D1", "Configuration D2", "Configuration D3"), 
    each = n()/3), 
    "n" = case_when(n == 255 ~ "(p + 1)", 
                   n == 500 ~ "(2p)", 
                   n == 1250 ~ "(5p)") |> 
      as_factor()
  ) |> 
  pivot_longer(-c(config, n), names_to = "method", values_to = "D") |> 
  mutate('method' = as_factor(method))

shade_df <- tribble(
  ~xmin, ~xmax, ~fill,
  0.5,   2.5,   "grey",   # PCA & PCA_T
  2.5,   4.5,   "white",   # SAVE & SAVE_T
  4.5,   6.5,   "grey",   # SIR-II & SIR_T
  6.5,   8.5,   "white"    # SSDR & SSDR_T
)

shade_data <- expand.grid(
  config = c("Configuration D1", "Configuration D2", "Configuration D3"),
  n = factor(c("(p + 1)", "(2p)", "(5p)"), levels = c("(p + 1)", "(2p)", "(5p)"))
) |> 
  crossing(shade_df)

sims |> 
  ggplot(aes(method, D, fill = method))+
  geom_rect(
    data = shade_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = NULL),
    inherit.aes = FALSE,
    fill = shade_data$fill,
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = .25, outlier.alpha = .25, linewidth = .25)+
  facet_grid(
    cols = vars(n), 
    rows = vars(config), 
    scales = "free_y",
    labeller = label_bquote(cols = n == H %.% .(as.character(n)))
  )+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176",
                               "lightgrey", "white"
  ))+
  scale_x_discrete(labels = c(
    "PCA", expression("PCA"["F"]), 
    "SAVE", expression("SAVE"["F"]), 
    "SIR-II", expression("SIR-II"["F"]), 
    "SSDR", expression("SSDR"["F"])
  ))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2))+
  guides(fill = "none")+
  theme(
    strip.background = element_rect(fill = "lightgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(), 
    legend.position = "bottom"
  )+
  labs(y = "Subspace Distance (D)")


# Golub_Application -------------------------------------------------------
load("saved_results/golub_sir2.RData")

df_train <- out$ProjectedData[,1] |> 
  as_tibble() |> 
  cbind(train_y) |> 
  set_names(c("x", "class")) |> 
  mutate(across(-class, function(x)(x - mean(x))/sd(x)))

df_test <- predict(out, newdata = test_x, type = "project", ndims = 1)$ProjectedData |>
  as_tibble() |>
  cbind(test_y) |>
  set_names(c("x", "class")) |> 
  mutate(across(-class, function(x)(x - mean(x))/sd(x)))

mesh_train <- df_train |>  
  with(
    expand.grid(
      "x" = seq(min(x) - 1.5, max(x) + 1, length.out = 501)
    )
  )
qda_mod <- MASS::qda(class ~., data = df_train)
mesh_train$class <- predict(qda_mod, mesh_train)$class

set.seed(1)
p1 <- ggplot(df_test, aes(x = x, y = 0, fill = class, shape = class)) +
  geom_raster(data = mesh_train, alpha = 1/3)+
  geom_jitter(size = 2)+
  scale_fill_manual(values = c("gold", "purple"))+
  scale_shape_manual(values = c(21, 24))+
  labs(y = "Jittered", x = "SIR-II 1st Subspace")+
  theme(legend.position = "none")

df_train_t <- t_out$ProjectedData[,1] |> 
  as_tibble() |> 
  cbind(train_y) |> 
  set_names(c("x", "class"))

df_test_t <- test_x %*% as.matrix(t_out$ProjectionMatrix[,1]) |> 
  as_tibble() |>
  cbind(test_y) |>
  set_names(c("x", "class"))

mesh_t_train <-  df_train_t |>  
  with(
    expand.grid(
      "x" = seq(min(x) - .1, max(x) + .1, length.out = 501)
    )
  )


qda_mod_t <- MASS::qda(class ~., data = df_train_t)
mean(df_test_t$class != predict(qda_mod_t, newdata = df_test_t)$class)
mesh_t_train$class <- predict(qda_mod_t, mesh_t_train)$class

set.seed(1)
p2 <- ggplot(df_test_t, aes(x = x, y = 0, fill = class, shape = class)) +
  geom_raster(data = mesh_t_train, alpha = 1/3)+
  geom_jitter(size = 2)+
  scale_fill_manual(values = c("gold", "purple"))+
  scale_shape_manual(values = c(21, 24))+
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())+
  labs(x = expression("SIR-II"["T"]*" 1st Subspace"), fill = "Leukemia", shape = "Leukemia")+
  theme(legend.position = "top")

(p1 + p2) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))


p5 <- ggplot(aes(1:length(out$eigvalues), out$eigvalues), data = NULL)+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = c(1, 1500, 3000, 4500, 7129))+
  labs(x = "SIR-II Subspace", y = "Eigenvalue Magnitude")

p6 <- ggplot(aes(1:length(t_out$dim_criteria), t_out$dim_criteria), data = NULL)+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = c(1, 1500, 3000, 4500, 7129), 
                     labels = t_out$dims[c(1, 1500, 3000, 4500, 7129)])+
  scale_y_continuous(breaks = round(c(0:4, t_out$dim_criteria[1]), 2))+
  labs(x = expression("SIR-II Subspaces Ordered by "*"T"["j"]), y = expression("T"["j"]))

p5 + p6



# Supplementary Material Sims ---------------------------------------------
load(file = "saved_results/config_S1_out.RData")
load(file = "saved_results/config_S2_out.RData")

LDA_sims <- config_S2_out |> 
  mutate(
    ni = case_when(ni == 51 ~ "p + 1", 
                   ni == 100 ~ "2p", 
                   ni == 250 ~ "5p") |> 
      as_factor(), 
    method = as_factor(method)
  )

QDA_sims <- config_S1_out |> 
  mutate(
    ni = case_when(ni == 51 ~ "p + 1", 
                   ni == 100 ~ "2p", 
                   ni == 250 ~ "5p") |> 
      as_factor(), 
    method = as_factor(method)
  )

lda_meds <- LDA_sims |>
  filter(method == "LDA") |>
  summarise("med" = median(CER), .by = ni)

qda_meds <- QDA_sims |>
  filter(method == "QDA") |>
  summarise("med" = median(CER), .by = ni)

shade_df <- tribble(
  ~xmin, ~xmax, ~fill,
  0.5,   2.5,   "grey",   # PCA & PCA_F
  2.5,   4.5,   "white",   # SAVE & SAVE_F
  4.5,   6.5,   "grey",   # SIR-II & SIR_F
  6.5,   8.5,   "white"    # SSDR & SSDR_F
)

shade_data <- expand.grid(
  ni = factor(c("p + 1", "2p", "5p"), levels = c("p + 1", "2p", "5p"))
) |> 
  crossing(shade_df)

LDA_sims |> 
  filter(method != "LDA") |> 
  ggplot(aes(method, CER, fill = method))+
  geom_rect(
    data = shade_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = NULL),
    inherit.aes = FALSE,
    fill = shade_data$fill,
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = .25, outlier.alpha = .25, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = "1"), data = lda_meds)+
  facet_wrap(~ni, 
             labeller = label_bquote(cols = n == .(as.character(ni))))+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176",
                               "lightgrey", "white"
  ))+
  scale_color_manual(values = "blue", labels = "LDA")+
  scale_x_discrete(labels = c(
    "PCA", expression("PCA"["F"]), 
    "SAVE", expression("SAVE"["F"]), 
    "SIR-II", expression("SIR-II"["F"]), 
    "SSDR", expression("SSDR"["F"])
  ))+
  guides(fill = "none")+
  theme(
    strip.background = element_rect(fill = "lightgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(), 
    legend.position = "bottom"
  )+
  labs(y = expression(widehat(CER)), color = "")

QDA_sims |> 
  filter(method != "QDA") |> 
  ggplot(aes(method, CER, fill = method))+
  geom_rect(
    data = shade_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = NULL),
    inherit.aes = FALSE,
    fill = shade_data$fill,
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = .25, outlier.alpha = .25, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = "1"), data = qda_meds)+
  facet_wrap(~ni, 
             labeller = label_bquote(cols = n[h] == .(as.character(ni))))+
  scale_fill_manual(values = c("purple", "#DAB1DA",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176",
                               "lightgrey", "white"
  ))+
  scale_color_manual(values = "red", labels = "QDA")+
  scale_x_discrete(labels = c(
    "PCA", expression("PCA"["F"]), 
    "SAVE", expression("SAVE"["F"]), 
    "SIR-II", expression("SIR-II"["F"]), 
    "SSDR", expression("SSDR"["F"])
  ))+
  guides(fill = "none")+
  theme(
    strip.background = element_rect(fill = "lightgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(), 
    legend.position = "bottom"
  )+
  labs(y = expression(widehat(CER)), color = "")


# Supplementary Real Data UCI ---------------------------------------------
sim_as_tb <- function(sim){
  out <- list()
  name <- names(sim) |> 
    stringr::str_split_i(pattern = "_", 1)
  for(i in 1:length(name)){
    out[[i]] <- lapply(sim[[i]], function(x){
      if(ncol(x) != 1) x <- x[,-ncol(x)]
      meds <- apply(x, 2, median)
      min_dim <- which.min(meds)
      x[, min_dim]
    }) |> 
      do.call(cbind, args = _) |> 
      as_tibble() |> 
      mutate("dataset" = rep(name[[i]], times = n()))
  }
  out |> 
    do.call(rbind, args =_)
}

data_appls <- miscset::lload("./saved_results", pattern = "app_out.RData") |> 
  sim_as_tb() |> 
  mutate(
    "dataset" = fct_relevel(as_factor(dataset),
                            "bc","divorce", "crx", "yeast") |>
      fct_recode("Breast Cancer" = "bc", "Divorce" = "divorce",
                 "Credit Score" = "crx", "Yeast" = "yeast")
  )

sims_long <-  data_appls |> 
  select(-QDA) |> 
  pivot_longer(-dataset, names_to = "method", values_to = "cer") |> 
  mutate("method" = fct_relevel(method,"DR", "DR_T", "SAVE","SAVE_T", "SIR-II","SIR-II_T", "SDRS","SDRS_T"))

meds_qda <- data_appls |> 
  summarise("med" = median(QDA), .by = dataset) |> 
  mutate("method" = rep("QDA", n()))

shade_df <- tribble(
  ~xmin, ~xmax, ~fill,
  0.5,   2.5,   "grey",   # DR & DR_T/F
  2.5,   4.5,   "white",   # SAVE & SAVE_T/F
  4.5,   6.5,   "grey",   # SIR-II & SIR-II_T/F
  6.5,   8.5,   "white"    # SSDR & SSDR_T/F
)

shade_data <- tibble(
  "dataset" = c("Breast Cancer", "Divorce", "Credit Score", "Yeast") |> as_factor()
) |> 
  crossing(shade_df)


p1 <- sims_long |> 
  filter(dataset %in% c("Breast Cancer", "Divorce")) |> 
  ggplot(aes(method, cer, fill = method))+
  geom_rect(
    data = shade_data |> filter(dataset %in% c("Breast Cancer", "Divorce")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = shade_data$fill[1:8],
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = method, linetype = method), data = meds_qda |> filter(dataset %in% c("Breast Cancer", "Divorce")))+
  facet_wrap(~dataset, scales = "free")+
  scale_color_manual(values = "red", labels = "QDA")+
  scale_fill_manual(values = c("#E66101", "#FDB863",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176"
  ))+
  scale_x_discrete(labels = c(
    "DR", expression("DR"["T"]), 
    "SAVE", expression("SAVE"["T"]), 
    "SIR-II", expression("SIR-II"["T"]), 
    "SSDR", expression("SSDR"["T"])
  ))+
  labs(y = expression(bar("CER")), 
       color = "", fill = "", linetype = "")+
  theme(strip.background = element_rect(fill="lightgrey"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        legend.position = "bottom"
  )+
  guides(fill = "none")

p2 <- sims_long |> 
  filter(dataset  == c("Credit Score")) |> 
  ggplot(aes(method, cer, fill = method))+
  geom_rect(
    data = shade_data |> filter(dataset %in% c("Credit Score")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = shade_data$fill[1:4],
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = method, linetype = method), data = meds_qda |> filter(dataset %in% c("Credit Score")))+
  facet_wrap(~dataset, scales = "free")+
  scale_color_manual(values = "red", labels = "QDA")+
  scale_fill_manual(values = c("#E66101", "#FDB863",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176"
  ))+
  scale_x_discrete(labels = c(
    "DR", expression("DR"["T"]), 
    "SAVE", expression("SAVE"["T"]), 
    "SIR-II", expression("SIR-II"["T"]), 
    "SSDR", expression("SSDR"["T"])
  ))+
  labs(y = expression(bar("CER")), 
       color = "", fill = "", linetype = "")+
  theme(strip.background = element_rect(fill="lightgrey"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        legend.position = "bottom"
  )+
  guides(fill = "none")

p3 <- sims_long |> 
  filter(dataset  == c("Yeast")) |> 
  ggplot(aes(method, cer, fill = method))+
  geom_rect(
    data = shade_data |> filter(dataset %in% c("Yeast")),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = shade_data$fill[1:4],
    alpha = 0.3
  )+
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, linewidth = .25)+
  geom_hline(aes(yintercept = med, color = method, linetype = method), data = meds_qda |> filter(dataset == c("Yeast")))+
  facet_wrap(~dataset, scales = "free")+
  scale_color_manual(values = "red", labels = "QDA")+
  scale_fill_manual(values = c("#E66101", "#FDB863",
                               "#21918c", "#52B9B3",
                               "#5ec962", "#A8E6A3",
                               "#fde725", "#FFF176"
  ))+
  scale_x_discrete(labels = c(
    "DR", expression("DR"["F"]), 
    "SAVE", expression("SAVE"["F"]), 
    "SIR-II", expression("SIR-II"["F"]), 
    "SSDR", expression("SSDR"["F"])
  ))+
  theme(strip.background = element_rect(fill="lightgrey"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(), 
        legend.position = "bottom"
  )+
  labs(color = "", fill = "", linetype = "", y = "")+
  guides(fill = "none")

p1 / (p2 + p3) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
