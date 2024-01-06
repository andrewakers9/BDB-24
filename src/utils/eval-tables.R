
library(kable)
library(kableExtra)

evaluate_ttt_nn <- function(model, val_df, feat_names) {
  
  ttt_val <- get_ttt_inputs(val_df, ttt_start_feats)
  val_preds <- predict(ttt_start_nn_mod, ttt_val$feats)
  
  crps <- mean(k_get_value(crps_loss(ttt_val$labels, val_preds)))
  
  val_preds_cum <- k_get_value(k_cumsum(val_preds, axis = 2))
  
  # median prediction 
  val_preds_num <- apply(val_preds_cum, 1, function(x) which(x > 0.5)[1])
  
  val_num_labels <- val_df %>%
    filter(
      between(time_to_tackle, 1, 100)
    ) %>%
    pull(time_to_tackle)
  mae <- mean(abs(val_num_labels - val_preds_num))
  
  return(list(crps = crps, mae = mae))
  
}

create_binary_eval_table <- function() {

  val_df <- read_csv("data/val_pred_df.csv")
  
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
      "Description" = "Tackle given attempt",
      "ppf_term" = "$P(Tackle_{i} | Attempt_{i} = 1, Z = \\textbf{z}_{i})$",
      "Target" = "tackle",
      "col_pred" = "tga_prob",
      "Method" = "XGBoost"
    ),
    list(
      "Description" = "Tackle given attempt at frame before tackle attempt",
      "ppf_term" = "$P(Tackle_{i} | T_{r} = T - 1, Attempt_{i} = 1, Z = \\textbf{z}_{i})$",
      "Target" = "tackle",
      "col_pred" = "tga_prob",
      "Method" = "XGBoost"
    )
  )
  
  evaluate_xgb_binary <- function(.list, val_df) {
  
    val_df <- val_df %>%
      mutate(
        truth = factor(!!sym(.list$Target))
      )
    
    if(.list$Target == "tackle") {
      val_df <- val_df %>% 
        filter(
          tackle_attempt == 1
      )
    }
    
    if(.list$Description == "Tackle given attempt at frame before tackle attempt") {
      val_df <- val_df %>%
        filter(
          time_to_tackle == 1
        )
    }
    
    val_df <- val_df %>%
      mutate(
        pred_factor = factor(if_else(!!sym(.list$col_pred) > 0.5, 1, 0))
      )
  
    .list$ll <- val_df %>%
      mn_log_loss(truth, !!sym(.list$col_pred), event_level = "second") %>%
      pull(.estimate) %>%
      round(3)
    .list$Precision <- val_df %>%
      precision(truth, pred_factor, event_level = "second") %>%
      pull(.estimate) %>%
      round(3)
    .list$AUC <- val_df %>%
      roc_auc(truth, !!sym(.list$col_pred), event_level = "second") %>%
      pull(.estimate) %>%
      round(3)
    
    return(.list)
    
  }
  
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



