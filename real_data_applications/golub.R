# data --------------------------------------------------------------------
{
  data('golub', package = 'datamicroarray')
  
  train_x <- 
    golub_train$x |> 
    scale()
  
  train_y <- golub_train$y
  
  test_x <- scale(golub_test$x, 
                  center = attr(train_x, "scaled:center"), 
                  scale = attr(train_x, "scaled:scale"))
  test_y <- golub_test$y
  rm(golub, golub_train, golub_test)
}

# SDR Fitting -------------------------------------------------------------
# Uncomment to run, took about 1 hours with Mac Air M2 
# {
#   start <- Sys.time()
#   out <- sdr::sdr(train_x, train_y, method = "sir2", regularize = TRUE)
#   Sys.time() - start
# }
# save(out, file = "saved_results/golub_sir2.RData")


# Analysis ----------------------------------------------------------------
## t and eig info
load(file = "saved_results/golub_sir2.RData")
t_out <- sdr::dim_order(object = out, method = "t2")
head(t_out$dims)
head(t_out$dim_criteria)
t_out$dim_criteria[which(t_out$dims == 1)]

## SIR-II results
df_train <- out$ProjectedData[,1] |> 
  as.data.frame() |> 
  cbind(train_y) |> 
  setNames(c("x", "class")) 

df_test <- predict(out, newdata = test_x, type = "project", ndims = 1)$ProjectedData |>
  as.data.frame() |> 
  cbind(test_y) |>
  setNames(c("x", "class"))

qda_mod <- MASS::qda(class ~., data = df_train)
mean(df_test$class != predict(qda_mod, newdata = df_test)$class)

## SIR-II_T results
df_train_t <- t_out$ProjectedData[,1] |> 
  as.data.frame() |> 
  cbind(train_y) |> 
  setNames(c("x", "class"))

df_test_t <- test_x %*% as.matrix(t_out$ProjectionMatrix[,1]) |> 
  as.data.frame() |> 
  cbind(test_y) |>
  setNames(c("x", "class"))

qda_mod_t <- MASS::qda(class ~., data = df_train_t)
mean(df_test_t$class != predict(qda_mod_t, newdata = df_test_t)$class)
