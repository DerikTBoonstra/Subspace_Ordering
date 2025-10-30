# Functions ---------------------------------------------------------------
sim_wrapper <- function(d, mu, Sigma, n, ni, nsims, dim.order = "t", methods = c("pca", "save", "sir2", "sdrs"), model = "lda"){
  start <- now()
  p <- length(mu[[1]])
  p_f <- progressor(along = seq_len(nsims*length(ni)))
  
  results <- future_lapply(seq_len(nsims), function(sim){
    
    set.seed(sim)
    # generating sample
    data <- lapply(seq_along(mu), function(i) {
      cbind("class" = i, MASS::mvrnorm(n, mu[[i]], Sigma[[i]])) |>
        as.data.frame()
    }) |>
      do.call(rbind, args = _) |> 
      mutate("class" = as_factor(class))
    
    lapply(ni, function(ni){
      set.seed(sim)
      
      ## LDA sampling is done by n = p + 1, not ni = p + 1
      if(model == "LDA"){
        class1_size <- ifelse(rnbinom(1, 1, .5) == 1, ceiling(ni/2), floor(ni/2))
        class2_size <- ni - class1_size
        
        train <- rbind(
          data[1:n, ][1:class1_size,],
          data[(n + 1):(2*n), ][1:class2_size,]
        )
        test <- rbind(
          data[1:n, ][-c(1:class1_size),],
          data[(n + 1):(2*n), ][-c(1:class2_size),]
        )
        
      } else {
        splits <- data |>
          rsample::initial_split(strata = class, prop = ni/n)
        train <- rsample::training(splits)
        test <- rsample::testing(splits)
      }
      
      methods <-   lapply(methods, function(method) {
        list(list(method, NULL), list(method, dim.order))
      }) |> unlist(recursive = FALSE)
      
      mods <- lapply(seq_along(methods), function(i){
        sdr::sdr(as.matrix(train[,-1]), y = train$class, method = methods[[i]][[1]], prec.est = NULL, dim.order = methods[[i]][[2]]
        )
      })
      
      if(tolower(model) == "lda"){
        err <- mean((MASS::lda(class~., data = train, tol = .Machine$double.eps^2) |>
                           predict(newdata = test))$class != test$class)
      } else if(tolower(model) == "qda") {
        err <- mean((MASS::qda(class~., data = train) |>
                       predict(newdata = test))$class != test$class)
      } else {
        stop(paste0('model = "lda" or model = "qda" not model = ', model))
      }
     
      
      temp <- lapply(seq_along(mods), function(i){
        sapply(d, function(r){
          pred <- predict(mods[[i]], ndims = r,
                          newdata = test[,-1], model = model, tol = .Machine$double.eps^2)$class
          mean(pred != test$class)
        })
      }) |>
        do.call(rbind, args = _) |>
        as.data.frame() |>
        rbind(rep(err, times = 1)) |>
        mutate(c("PCA", "PCA_T", "SAVE","SAVE_T", "SIR-II","SIR-II_T", "SDRS","SDRS_T",toupper(model)),
               rep(ni, times = length(methods) + 1)
        )
      p_f()
      temp
    }) |>
      do.call(rbind, args = _)
  }, future.seed = TRUE, future.stdout = FALSE) |>
    do.call(rbind, args = _) |>
    setNames(c(if (length(d) == 1) "CER" else paste0("CER", d), 
               "method", "ni"))
  print(now() - start)
  results
}

sim_wrapper_D <- function(beta, x_fn, y_fn, n, nsims, methods = c("pca", "save", "sir2", "sdrs"), H = 5){
  
  start <- now()
  p_f <- progressor(along = seq_len(nsims*length(n)))
  
  methods <- lapply(methods, function(method) {
    list(list(method, NULL), list(method, "F"))
  }) |> unlist(recursive = FALSE)
  
  subspace_distance <- function(A,B){
    if(is.vector(A)) A <- as.matrix(A)
    if(is.vector(B)) B <- as.matrix(B)
    Pa <- qr.Q(qr(A))
    Pa <- Pa %*% t(Pa)
    Pb <- qr.Q(qr(B))
    Pb <- Pb %*% t(Pb)
    d <- ncol(A)
    norm(Pa-Pb, type="F")/sqrt(2*d)
  }
  
  d <- ncol(beta)
  
  results <- future_lapply(1:nsims, function(sim){
    set.seed(sim)
    x <- x_fn(H*(max(n)))
    y <- y_fn(x, beta)
    
    lapply(H*n, function(ni){
      mods <- lapply(seq_along(methods), function(i){
        sdr::sdr(x[1:ni,], y = y[1:ni], method = methods[[i]][[1]], prec.est = NULL, dim.order = methods[[i]][[2]], 
                 ndims = d
        )
      })
      
      temp <- lapply(mods, function(x){
        subspace_distance(beta, x$ProjectionMatrix)
      }) |> 
        do.call(cbind, args = _) |> 
        as.data.frame() |> 
        setNames(c("PCA", "PCA_F", "SAVE", "SAVE_F", "SIR-II", "SIR-II_F", "SDRS", "SDRS_F")) |> 
        cbind("n" = ni)
      p_f()
      temp
    }) |> 
      do.call(rbind, args = _)
  }, future.seed = TRUE, future.stdout = FALSE) |> 
    do.call(rbind, args = _)
  print(now() - start)
  ## Return
  results
}

## Summarizes MC Sim by CER, SE and min dim
sim_summary <- function(sim){
  sim |>
    mutate("method" = as_factor(method)) |>
    group_by(ni) |>
    group_split() |>
    lapply(function(x){
      x <- x |>
        group_by(method) |>
        group_split() |>
        setNames(c("PCA", "PCA_T", "SAVE","SAVE_T", "SIR-II","SIR-II_T","SDRS","SDRS_T","QDA")) |>
        lapply(function(x) {
          x <- x |>
            select(-method, -ni)
          meds <- apply(x, 2 , median)
          ses <- apply(x, 2, sd)
          min_dim <- which.min(meds)
          paste0(round(meds[min_dim], 4)*100, "(", round(ses[min_dim], 5)*100, ")", "(", min_dim, ")")
        }) |>
        unlist()
    }) |>
    setNames(paste0("ni = ", unique(sim$ni)))
}

## Plots MC Sim as boxplots by dim
sim_boxplots <- function(sim, model = "lda"){
  qda_meds <- sim |>
    filter(method == toupper(model)) |>
    summarise("med" = median(CER), .by = ni)
  # lda_meds <- sim |>
  #   filter(method == "LDA") |>
  #   summarise("med" = median(`1`), .by = ni)
  
  sim |>
    mutate("method" = fct_relevel(method,"PCA", "PCA_T",  "SAVE","SAVE_T", "SIR-II","SIR-II_T", "SDRS","SDRS_T", toupper(model))) |>
    filter(method != toupper(model)) |>
    # filter(method != "LDA") |>
    pivot_longer(cols = -c(method, ni),
                 names_to = "dim",
                 values_to = "error") |>
    mutate("method" = as_factor(method),
           "dim" = as_factor(dim)) |>
    # filter(as.numeric(dim) %in% dims) |>
    ggplot(aes(method, error, fill =  method))+
    geom_boxplot(outlier.size = 0.25, outlier.alpha = 0.25)+
    geom_hline(aes(yintercept = med, color = "1"), data = qda_meds)+
    # geom_hline(aes(yintercept = med, color = "2"), data = lda_meds)+
    scale_color_manual(values = ifelse(model == "lda","blue", "red"), labels = toupper(model))+
    facet_wrap(~ni, ncol = 3, scales = "fixed")+
    scale_fill_manual(values = c("purple", "#DAB1DA",
                                 # "#6495ED", "#A0C4FF",
                                 "#21918c", "#52B9B3",
                                 "#5ec962", "#A8E6A3",
                                 "#fde725", "#FFF176",
                                 # ,
                                 "lightgrey", "white"
    ))+
    # scale_fill_manual()+
    labs(y = expression(widehat("CER")),
         x = "",
         color = "", fill = "")+
    theme(strip.background =element_rect(fill="lightgrey"),
          legend.position = "right", 
          axis.text.x = element_text(angle = 45, hjust = 1))+
    guides(fill = "none")
}

data_appl_wrapper <- function(data, nsims, dim.order = "t", ndims = ncol(data) - 1, methods = c("save", "sir2", "dr", "sdrs")){
  start <- now()
  p_f <- progressor(along = seq_len(nsims))
  
  sim <- future_lapply(seq_len(nsims), function(sim){
    set.seed(sim)
    
    # 10-fold CV split
    splits <- (data |> rsample::vfold_cv(strata = class, v = 10))$splits
    train <- lapply(splits, rsample::training)
    test <- lapply(splits, rsample::testing)
    
    methods <-   lapply(methods, function(method) {
      list(list(method, NULL), list(method, dim.order))
    }) |> unlist(recursive = FALSE)
    
    # SDR classifier model training
    mods <- lapply(seq_along(train), function(j){
      lapply(seq_along(methods), function(i){
        sdr::sdr(train[[j]][,-1], y = train[[j]]$class, method = methods[[i]][[1]], , prec.est = NULL, dim.order = methods[[i]][[2]]
        )
      })
    })
    
    # Predictions using QDA
    results <- lapply(seq_along(mods), function(j){
      lapply(seq_along(mods[[j]]), function(i){
        lapply(1:ndims, function(r){
          predict(mods[[j]][[i]], newdata = test[[j]][,-1], ndims = r, model = "qda", 
                  tol = .Machine$double.eps)$class
        })
      })
    })
    
    # Add QDA baseline
    for(j in seq_along(results)){
      results[[j]][[length(results[[j]]) + 1]] <- list(
        (MASS::qda(class~., data = train[[j]], tol = .Machine$double.eps) |> predict(newdata = test[[j]]))$class)
    }
    
    # Compute classification error rates (CER)
    errors <- lapply(seq_along(mods), function(k){
      lapply(seq_along(results[[k]]), function(i){
        lapply(seq_along(results[[k]][[i]]), function(j){
          mean(results[[k]][[i]][[j]] != test[[k]]$class)
        }) |> do.call(cbind, args = _)
      })
    })
    
    temp <- lapply(seq_along(errors[[1]]), function(i) {
      lapply(seq_along(errors), function(j) { errors[[j]][[i]] }) |> do.call(rbind, args = _)
    }) |> lapply(colMeans)
    
    
    p_f()
    temp
  }, future.seed = TRUE, future.stdout = FALSE)
  
  message(paste0("Time Difference of ", now() - start))
  
  sim <- lapply(seq_along(sim[[1]]), function(i){
    lapply(seq_along(sim), function(j){
      sim[[j]][[i]]
    }) |> do.call(rbind, args = _)
  })
  
  nms <- lapply(methods, function(x){
    x <- toupper(x)
    c(x, paste0(x, "_", toupper(dim.order)))
  }) |> unlist()
  names(sim) <- c(nms, "QDA")
  
  sim
}

data_app_summary <- function(sim){
  lapply(sim, function(x) {
    meds <- apply(x, 2 , median)
    ses <- apply(x, 2, sd)
    min_dim <- which.min(meds)
    paste0(round(meds[min_dim], 4)*100, "(", round(ses[min_dim], 5)*100, ")", "(", min_dim, ")")
  }) |>
    unlist()
}
