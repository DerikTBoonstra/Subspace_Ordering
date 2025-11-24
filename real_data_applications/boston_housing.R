# Packages ----------------------------------------------------------------
library("tidyverse");theme_set(theme_minimal());library("patchwork")

# Load data and Train/Test Split --------------------------------------------------------
data("BostonHousing", package = "mlbench")
BostonHousing <- BostonHousing |> 
  filter(crim < 3.2)
set.seed(1)
split <- rsample::initial_split(BostonHousing, prop = 0.80)
train <- rsample::training(split)
test <- rsample::testing(split)


# SDR Fitting -------------------------------------------------------------
out <- sdr::sdr(train[,-14], train$medv, method = "pca")
df_train <- tibble("y" = out$y, 
                   "x" = out$ProjectedData[,1])

f_out <- sdr::dim_order(object = out, method = "F")

# Analysis ----------------------------------------------------------------
## 11th PCA subspace is the most informative
f_out$dims 
f_out$dim_criteria

## creating dfs for projected test sets
df_train_f <- tibble("y" = out$y, 
                     "x" = f_out$ProjectedData[,1])
test_x <- apply(as.matrix(test[,-14]), 2, as.numeric)
df_test <- tibble("y" = test$medv, "x" = as.numeric(test_x %*% as.matrix(out$ProjectionMatrix[,1])))
df_test_f <- tibble("y" = test$medv, "x" = as.numeric(test_x %*% as.matrix(f_out$ProjectionMatrix[,1])))

## variability comparison
df_test$x |> var()
df_test_f$x |> var()

## Model fitting
mod <- lm(y~ x, data = df_train)
mod_f <- lm(y~ x, data = df_train_f)

## PCA
summary(mod)
Metrics::mse(df_test$y, predict(mod, newdata = df_test))


# PCA_F
summary(mod_f)
Metrics::mse(df_test_f$y, predict(mod_f, newdata = df_test_f))

# Plots -------------------------------------------------------------------
## Plot code given here instead of in figures.R for simplicity
p1 <-  df_test |> 
  ggplot(aes(x, y))+
  geom_point()+
  geom_line(aes(y = predict(mod, newdata = df_test)), color = "blue")+
  labs(x = "PCA 1st Subspace", y = "Median Housing Price")
  
p2 <- df_test_f |> 
  ggplot(aes(x, y))+
  geom_point()+
  geom_line(aes(y = predict(mod_f, newdata = df_test_f)), color = "red", linetype = "dashed")+
  labs(x = expression("PCA"["F"]*" 1st Subspace"))+
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank())
p1 + p2

