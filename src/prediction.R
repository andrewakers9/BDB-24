
create_tr_model <- function() {
  tr_nn_mod <- keras_model_sequential() %>%
    layer_dense(32, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(100, activation = "softmax")
  return(tr_nn_mod)
}

get_calibrated_tr_probs <- function(model, input_list) {
  
  n_time_steps <- ncol(input_list$labels)
  preds <- predict(model, input_list$feats)
  
  map_f <- function(i, input_list, preds) {
    if(i != 1) {
      labels <- input_list$labels[, i] - input_list$labels[, i - 1]
    } else {
      labels <- input_list$labels[, i]
    }
    iso_reg <- as.stepfun(isoreg(preds[, i], labels))
    return(iso_reg(preds[, i]))
  }
  
  out <- future_map(1:n_time_steps, map_f, input_list, preds)
  out <- do.call(cbind, out)
  
  # normalize so each rows adds up to 1
  pred_norm <- 1 / rowSums(out)
  for(i in 1:length(pred_norm)) {
    out[i, ] <- out[i, ] * pred_norm[i]
  }
  out <- k_get_value(k_cumsum(out, axis = 2))
  
  return(out)  
  
}

extract_tr_probs <- function(model, df, feat_names) {
  
  # fit and save calibration models 
  cal_df <- df %>%
    filter(
      between(time_to_end, 1, 100)
    )
  tr_inputs <- get_tr_inputs(cal_df, feat_names)
  preds <- predict(model, tr_inputs$feats)
  
  n_time_steps <- ncol(preds)
  
  map_f <- function(i, preds, tr_inputs) {
    if(i != 1) {
      labels <- tr_inputs$labels[, i] - tr_inputs$labels[, i - 1]
    } else {
      labels <- tr_inputs$labels[, i]
    }
    return(as.stepfun(isoreg(preds[, i], labels)))
  }
  cal_models <- future_map(1:n_time_steps, map_f, preds, tr_inputs)
  
  # features and preds for full dataset 
  all_feats <- get_tr_inputs(df, feat_names, inference = TRUE)
  all_preds <- predict(model, all_feats)
  
  # calibrate preds with save models 
  for(i in 1:n_time_steps) {
    all_preds[, i] <- cal_models[[i]](all_preds[, i])
  }
  
  # normalize so each rows adds up to 1
  pred_norm <- 1 / rowSums(all_preds)
  for(i in 1:length(pred_norm)) {
    all_preds[i, ] <- all_preds[i, ] * pred_norm[i]
  }
  all_preds <- k_get_value(k_cumsum(all_preds, axis = 2))
  
  times <- df$time_to_end
  times[times < 0] <- 1
  times[times > 100] <- 100
  
  # determine indexes to extract from flattened preds based on time 
  idxs <- 0:(nrow(all_preds) - 1) * n_time_steps + times 
  
  out <- as.numeric(t(all_preds))[idxs]
  
  return(out)
  
}


add_preds_to_df <- function(xgb_model, 
                            df, 
                            feat_names,
                            pred_name = "pred",
                            filters = list()) {
  
  df <- df %>%
    mutate(idx = row_number())
  
  if(length(filters) > 0) {
    filt_df <- df
    for(i in 1:length(filters)) {
      filt_df <- filt_df %>%
        filter(!!parse_expr(filters[[i]]))
    }
  } else {
    filt_df <- df
  }
  
  dmat <- filt_df %>%
    select(all_of(feat_names)) %>%
    as.matrix() %>%
    xgb.DMatrix()
  
  if(length(filters) > 0) {
    
    preds <- predict(xgb_model, dmat)
    df[[pred_name]] <- NA
    df[[pred_name]][filt_df$idx] <- preds
    
  } else {
    
    df[[pred_name]] <- predict(xgb_model, dmat)
    
  }
  
  df$idx <- NULL
  
  return(df)
  
}

## main prediction function
run_predictions <- function() {
  
  val_df <- read_csv("data/val_df.csv", col_types = cols())
  feat_list <- read_rds("data/feat_list.rds")
  
  tr_xgb_mod <- xgb.load("models/tr_xgb.model")
  val_df <- add_preds_to_df(
    tr_xgb_mod,
    val_df,
    feat_list$tr_feats,
    pred_name = "tr_pred"
  )
  
  tr_nn_mod <- create_tr_model()
  load_model_weights_tf(tr_nn_mod, "models/tr_nn")
  tr_feats <- append(feat_list$tr_feats, "tr_pred")
  tr_val <- get_tr_inputs(val_df, tr_feats)
  val_df$tr_prob <- extract_tr_probs(tr_nn_mod, val_df, tr_feats)
  
  tr_start_xgb_mod <- xgb.load("models/tr_start_xgb.model")
  val_df <- add_preds_to_df(
    tr_start_xgb_mod,
    val_df,
    feat_list$tr_start_feats,
    pred_name = "tr_start_pred"
  )
  
  tr_start_nn_mod <- create_tr_model()
  load_model_weights_tf(tr_start_nn_mod, "models/tr_start_nn")
  tr_start_feats <- append(feat_list$tr_start_feats, "tr_start_pred")
  tr_val <- get_tr_inputs(val_df, tr_start_feats)
  val_df$tr_start_prob <- extract_tr_probs(tr_start_nn_mod, val_df, tr_start_feats)
  
  ta_start_xgb_mod <- xgb.load("models/ta_start_xgb.model")
  val_df <- add_preds_to_df(
    ta_start_xgb_mod,
    val_df,
    feat_list$ta_start_feats,
    pred_name = "ta_start_prob"
  )
  
  ta_xgb_mod <- xgb.load("models/ta_xgb.model")
  val_df <- add_preds_to_df(
    ta_xgb_mod,
    val_df,
    feat_list$ta_feats,
    pred_name = "ta_prob"
  )
  
  tga_xgb_mod <- xgb.load("models/tga_xgb.model")
  val_df <- add_preds_to_df(
    tga_xgb_mod,
    val_df,
    feat_list$tga_feats,
    pred_name = "tga_prob"
  )
  
  gamma_glm <- read_rds("models/gamma_glm.rds")
  
  # extract Gamma rate and shape parameters for each row
  ALPHA <- gamma_glm$gamma_shape
  val_df <- val_df %>%
    mutate(
      closest_def = if_else(proximity_rank == 1, "first", 
                            if_else(proximity_rank == 2, "second", "a")),
      closest_def_start = if_else(proximity_rank_start == 1, "first",
                                  if_else(proximity_rank_start == 2, "second", "a"))
    ) %>%
    mutate(
      gamma_rate = ALPHA / predict(gamma_glm, ., type = "response")
    ) 
  
  # calculate T <= t from Gamma CDF, then use Bayes' theorem to get conditional tackle attempt probs
  val_df <- val_df %>%
    mutate(
      cdf_cond = map2_dbl(time_to_end, gamma_rate, ~ pgamma(.x, ALPHA, rate = .y)),
      ta_cond_prob = (tr_prob * ta_prob) / cdf_cond,
      ta_start_cond_prob = (tr_start_prob * ta_start_prob) / cdf_cond
    )
  
  gamma_glm <- read_rds("models/gamma_glm_start.rds")
  ALPHA <- gamma_glm$gamma_shape
  val_df <- val_df %>%
    mutate(
      gamma_rate = ALPHA / predict(gamma_glm, ., type = "response")
    ) %>%
    mutate(
      cdf_cond_start = map2_dbl(time_to_end, gamma_rate, ~ pgamma(.x, ALPHA, rate = .y)),
      ta_start_cond_prob = (tr_start_prob * ta_start_prob) / cdf_cond_start
    ) 
  
  # ensure probs are in the range [0, 1]
  val_df <- val_df %>%
    mutate(
      ta_cond_prob = if_else(ta_cond_prob > 1, 1, ta_cond_prob),
      ta_cond_prob = if_else(time_to_end == 0 & tackle_attempt == 0, 0, ta_cond_prob),
      ta_start_cond_prob = if_else(ta_start_cond_prob > 1, 1, ta_start_cond_prob),
      ta_start_cond_prob = if_else(time_to_end == 0 & tackle_attempt == 0, 0, ta_start_cond_prob)
    )
  
  val_df <- val_df %>%
    select(
      game_id,
      play_id,
      nfl_id,
      frame_id,
      time_to_end,
      time_to_tackle,
      tackle,
      tackle_attempt,
      cdf_cond,
      contains("_prob"),
      contains("_pred")
    ) %>%
    filter(time_to_end != 0)
  
  # calibrate conditional tackle attempt probabilities 
  iso_reg <- isoreg(val_df$ta_cond_prob, val_df$tackle_attempt)
  val_df$ta_cond_prob_cal <- as.stepfun(iso_reg)(val_df$ta_cond_prob)
  
  iso_reg <- isoreg(val_df$ta_start_cond_prob, val_df$tackle_attempt)
  val_df$ta_start_cond_prob_cal <- as.stepfun(iso_reg)(val_df$ta_start_cond_prob)
  
  # calibrate tackle given attempt probabilities
  tmp <- val_df %>%
    filter(time_to_tackle == 1)
  iso_reg <- isoreg(tmp$tga_prob, tmp$tackle)
  val_df$tga_prob_cal <- as.stepfun(iso_reg)(val_df$tga_prob)
  
  write_csv(val_df, "data/val_pred_df.csv")
  
  return(
    invisible(NULL)
  )
  
}
