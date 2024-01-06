library(keras)
library(kableExtra)
library(yardstick)
library(tidyverse)
source("src/model-training.R")
source("src/prediction.R")

# python env for keras backend 
reticulate::use_condaenv("tf-keras")

evaluate_tr_nn <- function(model_fp, feat_names) {
  
  val_df <- read_csv("data/val_df.csv", col_types = cols()) %>%
    filter(
      between(time_to_end, 1, 100),
      tackle_attempt == 1
    )
  val_preds_df <- read_csv("data/val_pred_df.csv", col_types = cols()) %>%
    filter(
      between(time_to_end, 1, 100),
      tackle_attempt == 1
    ) %>%
    select(tr_start_pred, tr_pred)
  val_df <- val_df %>%
    bind_cols(val_preds_df)
  
  model <- create_tr_model()
  load_model_weights_tf(model, model_fp)
  
  tr_val <- get_tr_inputs(val_df, feat_names)
  val_preds <- predict(model, tr_val$feats)
  
  crps <- mean(k_get_value(crps_loss(tr_val$labels, val_preds)))
  
  val_preds_cum <- k_get_value(k_cumsum(val_preds, axis = 2))
  
  # median prediction 
  val_preds_num <- apply(val_preds_cum, 1, function(x) which(x > 0.5)[1])
  
  val_num_labels <- val_df %>%
    pull(time_to_end)
  mae <- mean(abs(val_num_labels - val_preds_num))
  
  return(list(crps = crps, mae = mae))
  
}

evaluate_gamma_glm <- function(model_fp) {

  model <- read_rds(model_fp)
  df <- read_csv("data/val_df.csv", col_types = cols()) %>%
    filter(time_to_end > 0) %>%
    mutate(
      time_to_end = if_else(time_to_end > 100, 100, time_to_end)
    )
  pred_df <- read_csv("data/val_pred_df.csv", col_types = cols())
  
  df <- df %>%
    mutate(
      tr_pred = pred_df$tr_pred,
      tr_start_pred = pred_df$tr_start_pred,
      closest_def = if_else(proximity_rank == 1, "first", 
                            if_else(proximity_rank == 2, "second", "a")),
      closest_def_start = if_else(proximity_rank_start == 1, "first",
                                  if_else(proximity_rank_start == 2, "second", "a"))
    )
  glm_preds <- predict(model, df, type = "response")
  names(glm_preds) <- NULL
  mae <- mean(abs(df$time_to_end - glm_preds))
  
  ALPHA <- model$gamma_shape
  gamma_rates <- ALPHA / glm_preds
  
  cdf_preds <- matrix(0, nrow(df), 100)
  for(i in 1:nrow(df)) {
    cdf_preds[i, ] <- pgamma(1:100, ALPHA, gamma_rates[i])
  }
  
  label_mat <- matrix(0, nrow(df), 100)
  for(i in 1:nrow(label_mat)) {
    label_mat[i, df$time_to_end[i]:ncol(label_mat)] <- 1
  }
  
  crps <- mean((label_mat - cdf_preds)^2)
  
  return(
    list(crps = crps, mae = mae)
  )
  
}

evaluate_xgb_binary <- function(x, val_df) {
  
  val_df <- val_df %>%
    mutate(
      truth = factor(!!sym(x$Target))
    )
  
  if(x$Target == "tackle") {
    val_df <- val_df %>% 
      filter(
        tackle_attempt == 1
      )
  }
  
  if(x$Description == "Tackle given attempt at frame before tackle attempt") {
    val_df <- val_df %>%
      filter(
        time_to_tackle == 1
      )
  }
  
  val_df <- val_df %>%
    mutate(
      pred_factor = factor(if_else(!!sym(x$col_pred) > 0.5, 1, 0))
    )
  
  x$ll <- val_df %>%
    mn_log_loss(truth, !!sym(x$col_pred), event_level = "second") %>%
    pull(.estimate) %>%
    round(3)
  x$Precision <- val_df %>%
    precision(truth, pred_factor, event_level = "second") %>%
    pull(.estimate) %>%
    round(3)
  x$AUC <- val_df %>%
    roc_auc(truth, !!sym(x$col_pred), event_level = "second") %>%
    pull(.estimate) %>%
    round(3)
  
  return(x)
  
}

evaluate_cdf_model <- function(x) {
  
  if(x$Method == "Neural network") { 
    feat_name_list <- read_rds("data/feat_list.rds")
    if(x$fp == "tr_start_nn") {
      feat_names <- append(feat_name_list$tr_start_feats, "tr_start_pred")
    } else {
      feat_names <- append(feat_name_list$tr_feats, "tr_pred")
    }
    
    fp <- paste0("models/", x$fp)
    metrics <- evaluate_tr_nn(fp, feat_names)
    
    x$crps <- metrics$crps
    x$mae <- metrics$mae
    
  } else {
    metrics <- evaluate_gamma_glm(paste0("models/", x$fp))
    x$crps <- metrics$crps
    x$mae <- metrics$mae
  }
  
  return(x)
  
}


create_binary_eval_table <- function() {
  
  model_list <- list(
    list(
      "Description" = "Tackle attempt using starting features",
      "ppf_term" = "$P(Attempt_{i} | T_{e} = t_{e}, X = \\textbf{x}_{i})$",
      "Target" = "tackle_attempt",
      "col_pred" = "ta_start_prob",
      "Method" = "XGBoost"
    ),
    list(
      "Description" = "Tackle attempt using starting features conditioned on time remaining",
      "ppf_term" = "$P(Attempt_{i} | T_{r}\\leq t_{r}, T_{e} = t_{e}, X = \\textbf{x}_{i})$",
      "Target" = "tackle_attempt",
      "col_pred" = "ta_start_cond_prob",
      "Method" = "Bayes' Theorem"
    ),
    list(
      "Description" = "Tackle attempt using current features",
      "ppf_term" = "$P(Attempt_{i} | T_{e} = t_{e}, Z = \\textbf{z}_{i})$",
      "Target" = "tackle_attempt",
      "col_pred" = "ta_prob",
      "Method" = "XGBoost"
    ),
    list(
      "Description" = "Tackle attempt using current features conditioned on time remaining",
      "ppf_term" = "$P(Attempt_{i} | T_{r}\\leq t_{r}, T_{e} = t_{e}, Z = \\textbf{z}_{i})$",
      "Target" = "tackle_attempt",
      "col_pred" = "ta_cond_prob",
      "Method" = "Bayes' Theorem"
    ),
    list(
      "Description" = "Tackle given attempt at frame before tackle attempt",
      "ppf_term" = "$P(Tackle_{i} | T_{r} = T - 1, Attempt_{i} = 1, Z = \\textbf{z}_{i})$",
      "Target" = "tackle",
      "col_pred" = "tga_prob",
      "Method" = "XGBoost"
    )
  )
  
  val_df <- read_csv("data/val_pred_df.csv", col_types = cols())
  
  table_df <- map_df(model_list, evaluate_xgb_binary, val_df)
  table_df %>%
    select(-Target, -col_pred) %>%
    rename(
      `PPF Term` = ppf_term,
      `Log loss` = ll) %>%
    kbl() %>%
    add_header_above(
      c(" " = 3, "Performance metric" = 3)
    ) %>%
    kable_material("striped", protect_latex = TRUE) 
  
}

create_cdf_eval_table <- function() {

  model_list <- list(
    list(
      "Description" = "Time remaining CDF given attempt using starting features",
      "ppf_term" = "$P(T_{r} \\leq t_{r} | TA_{i} = 1, T_{e} = t_{e}, X = \\textbf{x}_{i})$",
      "fp" = "tr_start_nn",
      "Method" = "Neural network"
    ),
    list(
      "Description" = "Time remaining CDF using starting features",
      "ppf_term" = "$P(T_{r} \\leq t_{r} | T_{e} = t_{e}, X = \\textbf{x}_{i})$",
      "fp" = "gamma_glm_start.rds",
      "Method" = "Gamma GLM"
    ),
    list(
      "Description" = "Time remaining CDF given attempt using current features",
      "ppf_term" = "$P(T_{r} \\leq t_{r} | TA_{i} = 1, T_{e} = t_{e}, Z = \\textbf{z}_{i})$",
      "fp" = "tr_nn",
      "Method" = "Neural network"
    ),
    list(
      "Description" = "Time remaining CDF using current features",
      "ppf_term" = "$P(T_{r} \\leq t_{r} | T_{e} = t_{e}, Z = \\textbf{z}_{i})$",
      "fp" = "gamma_glm.rds",
      "Method" = "Gamma GLM"
    )
  )
  
  table_df <- map_df(model_list, evaluate_cdf_model)
  table_df %>%
    select(-fp) %>%
    rename(
      `PPF Term` = ppf_term,
      CRPS = crps,
      MAE = mae
    ) %>%
    mutate(
      across(CRPS:MAE, ~ round(.x, 3))
    ) %>%
    kbl() %>%
    add_header_above(
      c(" " = 3, "Performance metric" = 2)
    ) %>%
    kable_material("striped", protect_latex = TRUE) 
  
}

create_binary_eval_table()
create_cdf_eval_table()


