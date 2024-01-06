
train_xgb <- function(train_df, 
                      val_df,
                      feat_names,
                      label_name,
                      params,
                      filters = list(),
                      nrounds = 300,
                      early_stopping_rounds = 10) {
  
  if(length(filters) > 0) {
    for(i in 1:length(filters)) {
      train_df <- train_df %>%
        filter(!!parse_expr(filters[[i]]))
      val_df <- val_df %>%
        filter(!!parse_expr(filters[[i]]))
    }
  }
  
  train_label <- train_df[[label_name]]
  val_label <- val_df[[label_name]]
  
  train_dmat <- train_df %>%
    select(all_of(feat_names)) %>%
    as.matrix() %>%
    xgb.DMatrix(label = train_label)
  
  val_dmat <- val_df %>%
    select(all_of(feat_names)) %>%
    as.matrix() %>%
    xgb.DMatrix(label = val_label)
  
  mod <- xgb.train(
    params = params,
    data = train_dmat,
    watchlist = list(train = train_dmat, val = val_dmat),
    nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds
  )
  
  return(mod)
  
}

get_tr_inputs <- function(df, 
                          feat_names, 
                          inference = FALSE,
                          n_time_steps = 100) {
  
  if(!inference) {
    df <- df %>%
      filter(
        tackle_attempt == 1,
        between(time_to_end, 1, n_time_steps)
      )
  }
  
  feat_mat <- df %>%
    select(all_of(feat_names)) %>%
    mutate(
      across(everything(), ~ (.x - min(.x)) / (max(.x) - min(.x)))
    ) %>%
    as.matrix()
  
  if(inference) return(feat_mat)
  
  label_mat <- matrix(0, nrow(df), 100)
  for(i in 1:nrow(label_mat)) {
    label_mat[i, df$time_to_end[i]:ncol(label_mat)] <- 1
  }
  
  return(
    list(
      feats = feat_mat,
      labels = label_mat
    )
  )
  
}

crps_loss <- function(y_true, y_pred) {
  y_pred <- k_cumsum(y_pred, axis = 2)
  loss <- k_mean((y_true - y_pred)^2, axis = -1)
  return(loss)
}

reduce_glm_obj_size <- function(glm_obj) {
  glm_obj$model <- NULL
  glm_obj$y <- NULL
  glm_obj$data <- NULL
  glm_obj$residuals <- NULL
  glm_obj$fitted.values <- NULL
  glm_obj$effects <- NULL
  glm_obj$weights <- NULL
  glm_obj$prior.weights <- NULL
  glm_obj$linear.predictors <- NULL
  return(glm_obj)
}

## main model function
train_all_models <- function() {
  
  train_df <- read_csv("data/train_df.csv", col_types = cols())
  val_df <- read_csv("data/val_df.csv", col_types = cols())
  
  # save features in list
  feat_list <- list()
  
  # time remaining at current frame xgb regression model 
  cat("Training XGBoost time to tackle model with features from current frame...\n")
  tr_feats <- c(
    "pass_play",
    "rel_x",
    "rel_y",
    "speed_x",
    "speed_y",
    "acc",
    "rel_s_x",
    "rel_s_y",
    "rel_x_d",
    "rel_y_d",
    "rel_s_x_d",
    "rel_s_y_d",
    "rel_x_off",
    "rel_y_off",
    "acc_bc",
    "y_bc"
  )
  feat_list$tr_feats <- tr_feats

  tr_filters <- list(
    "time_to_end > 0",
    "time_to_end <= 100",
    "tackle_attempt == 1"
  )
  
  params <- list(
    objective = "reg:absoluteerror",
    eta = 0.4,
    max_depth = 6,
    colsample_bytree = 0.8,
    gamma = 2,
    min_child_weight = 1
  )
  
  tr_xgb_mod <- train_xgb(
    train_df,
    val_df,
    tr_feats,
    label_name = "time_to_end",
    params,
    tr_filters,
    nrounds = 500
  )
  xgb.save(tr_xgb_mod, "models/tr_xgb.model")
  
  train_df <- add_preds_to_df(
    tr_xgb_mod,
    train_df,
    tr_feats,
    pred_name = "tr_pred"
  )
  
  val_df <- add_preds_to_df(
    tr_xgb_mod,
    val_df,
    tr_feats,
    pred_name = "tr_pred"
  )
  
  # time remaining given attempt cumulative dist model trained with crps loss 
  cat("Training time remaining CDF neural net with current frame features...\n")
  tr_feats <- append(tr_feats, "tr_pred")
  
  tr_train <- get_tr_inputs(train_df, tr_feats)
  tr_val <- get_tr_inputs(val_df, tr_feats)
  
  tr_nn_mod <- keras_model_sequential() %>%
    layer_dense(32, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(100, activation = "softmax")
  
  tr_nn_mod %>% 
    compile(
      loss = crps_loss,
      optimizer = optimizer_adam(learning_rate = 0.00001)
    )
  keras_fit <- tr_nn_mod %>%
    fit(
      x = tr_train$feats,
      y = tr_train$labels,
      validation_data = tr_val,
      epochs = 75,
      batch_size = 64
    )
  save_model_weights_tf(tr_nn_mod, "models/tr_nn")
  
  ################################################################################
  # time remaining with fixed start location
  cat("Training time remaining XGBoost with starting frame features...\n")
  tr_start_feats <- c(
    "pass_play",
    "frame_id_adj",
    "rel_x_start",
    "rel_y_start",
    "rel_s_x_start",
    "rel_s_y_start",
    "speed_x_bc",
    "speed_y_bc",
    "acc_bc",
    "y_bc",
    "rel_x_off",
    "rel_y_off"
  )
  feat_list$tr_start_feats <- tr_start_feats
  
  params <- list(
    objective = "reg:absoluteerror",
    eta = 0.4,
    max_depth = 6,
    colsample_bytree = 0.8,
    gamma = 2,
    min_child_weight = 2
  )
  
  tr_start_xgb_mod <- train_xgb(
    train_df,
    val_df,
    tr_start_feats,
    label_name = "time_to_end",
    params,
    tr_filters,
    nrounds = 400
  )
  xgb.save(tr_start_xgb_mod, "models/tr_start_xgb.model")
  
  train_df <- add_preds_to_df(
    tr_start_xgb_mod,
    train_df,
    tr_start_feats,
    pred_name = "tr_start_pred"
  )
  
  val_df <- add_preds_to_df(
    tr_start_xgb_mod,
    val_df,
    tr_start_feats,
    pred_name = "tr_start_pred"
  )
  
  # time to tackle given attempt cumulative dist model trained with crps loss 
  cat("Training time remaining CDF neural net with starting frame features...\n")
  tr_start_feats <- append(tr_start_feats, "tr_start_pred")
  
  tr_train <- get_tr_inputs(train_df, tr_start_feats)
  tr_val <- get_tr_inputs(val_df, tr_start_feats)
  
  tr_start_nn_mod <- keras_model_sequential() %>%
    layer_dense(32, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(100, activation = "softmax")
  
  tr_start_nn_mod %>% 
    compile(
      loss = crps_loss,
      optimizer = optimizer_adam(learning_rate = 0.00001)
    )
  keras_fit <- tr_start_nn_mod %>%
    fit(
      x = tr_train$feats,
      y = tr_train$labels,
      validation_data = tr_val,
      epochs = 30,
      batch_size = 64
    )
  save_model_weights_tf(tr_start_nn_mod, "models/tr_start_nn")
  
  # tackle attempt model at the start of the play
  cat("Training tackle attempt model using starting frame features...\n")
  ta_start_feats <- c(
    "pass_play",
    "frame_id_adj",
    "rel_x_start",
    "rel_y_start",
    "rel_s_x_start",
    "rel_s_y_start",
    "proximity_rank_start",
    "rel_x_d1",
    "rel_y_d1",
    "rel_s_x_d1",
    "rel_s_y_d1",
    "speed_x_bc",
    "speed_y_bc",
    "yards_to_goal",
    "y_bc",
    "rel_x_off",
    "rel_y_off",
    "rel_los_x"
  )
  feat_list$ta_start_feats <- ta_start_feats
  
  params <- list(
    objective = "binary:logistic",
    eta = 0.3,
    max_depth = 5,
    colsample_bytree = 0.8,
    gamma = 2,
    min_child_weight = 1
  )
  
  ta_start_xgb_mod <- train_xgb(
    train_df,
    val_df,
    ta_start_feats,
    label_name = "tackle_attempt",
    params
  )
  xgb.save(ta_start_xgb_mod, "models/ta_start_xgb.model")
  
  # tackle attempt model 
  cat("Training tackle attempt model using features from current frame...\n")
  ta_feats <- c(
    "pass_play",
    "rel_los_x",
    "rel_x",
    "rel_y",
    "speed_x",
    "speed_y",
    "acc",
    "rel_s_x",
    "rel_s_y",
    "proximity_rank",
    "rel_x_d1",
    "rel_y_d1",
    "acc_bc",
    "yards_to_goal",
    "y_bc",
    "rel_x_off",
    "rel_y_off"
  )
  feat_list$ta_feats <- ta_feats
  
  params <- list(
    objective = "binary:logistic",
    eta = 0.3,
    max_depth = 6,
    colsample_bytree = 0.8,
    gamma = 3,
    min_child_weight = 1
  )
  
  ta_xgb_mod <- train_xgb(
    train_df,
    val_df,
    ta_feats,
    label_name = "tackle_attempt",
    params
  )
  xgb.save(ta_xgb_mod, "models/ta_xgb.model")
  
  # tackle given attempt model 
  cat("Training tackle given attempt model...\n")
  tga_feats <- c(
    "pass_play",
    "rel_los_x",
    "rel_x",
    "rel_y",
    "speed_x",
    "speed_y",
    "acc",
    "rel_s_x",
    "rel_s_y",
    "rel_x_d",
    "rel_y_d",
    "rel_s_x_d",
    "rel_s_y_d",
    "acc_bc",
    "y_bc"
  )
  feat_list$tga_feats <- tga_feats
  write_rds(feat_list, "data/feat_list.rds")
  
  tga_filters <- list(
    "tackle_attempt == 1",
    "time_to_tackle == 1"
  )
  
  params <- list(
    objective = "binary:logistic",
    eta = 0.2,
    max_depth = 6,
    colsample_bytree = 0.8,
    gamma = 2,
    min_child_weight = 2
  )
  
  tga_xgb_mod <- train_xgb(
    train_df,
    val_df,
    tga_feats,
    label_name = "tackle",
    params,
    tga_filters
  )
  xgb.save(tga_xgb_mod, "models/tga_xgb.model")
  
  # fit gamma dist to play times to get unconditional time probs 
  train_df <- train_df %>%
    mutate(
      closest_def = if_else(proximity_rank == 1, "first", 
                            if_else(proximity_rank == 2, "second", "a")),
      closest_def_start = if_else(proximity_rank_start == 1, "first",
                                  if_else(proximity_rank_start == 2, "second", "a"))
    ) %>%
    filter(time_to_end > 0)
    
  gamma_glm <- glm(
    time_to_end ~ frame_id_adj + closest_def*tr_pred,
    family = Gamma(link = "log"),
    data = train_df
  )
  gamma_glm$gamma_shape <- MASS::gamma.shape(gamma_glm)$alpha
  gamma_glm <- reduce_glm_obj_size(gamma_glm)
  write_rds(gamma_glm, "models/gamma_glm.rds")
  
  gamma_glm <- glm(
    time_to_end ~ frame_id_adj + closest_def_start*tr_start_pred,
    family = Gamma(link = "log"),
    data = train_df
  )
  gamma_glm$gamma_shape <- MASS::gamma.shape(gamma_glm)$alpha
  gamma_glm <- reduce_glm_obj_size(gamma_glm)
  write_rds(gamma_glm, "models/gamma_glm_start.rds")
  
  return(
    invisible(NULL)
  )

}

