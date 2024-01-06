library(MASS)
library(xgboost)
library(reticulate)
library(keras)
library(tensorflow)
library(future)
library(furrr)
library(rlang)
library(yardstick)
library(janitor)
library(tidyverse)

source("src/preprocess-data.R")
source("src/model-training.R")
source("src/prediction.R")
#source("src/evaluation.R")

# global conflict preferences
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
conflicted::conflict_prefer_all("tidyr", quiet = TRUE)

# python env for keras backend 
reticulate::use_condaenv("tf-keras")
# sets random seed for R and Keras 
tensorflow::set_random_seed(2024)

# set multi processing strategy
future::plan(strategy = "cluster", workers = 5)

# save train and val features data frames
preprocess()
gc()

# train and save all models 
train_all_models()
gc()

# add predicted probabilities to val data frame
run_predictions()
gc()

################################################################################

val_pred_df <- read_csv("data/val_pred_df.csv", col_types = cols())
players_df <- read_csv("data/players.csv", col_types = cols()) %>%
  janitor::clean_names()

# get unconditional expectation of tackle prob given attempt 
AVG_TACKLE_PROB <- val_pred_df %>%
  distinct(game_id, play_id, nfl_id, .keep_all = TRUE) %>%
  filter(tackle_attempt == 1) %>%
  pull(tackle) %>%
  mean()

# get appropriate tackle pred relative to the tackle frame
val_pred_df <- val_pred_df %>%
  left_join(
    val_pred_df %>%
      filter(time_to_tackle == 1) %>%
      select(
        game_id,
        play_id,
        nfl_id,
        tga_prob_at_tackle = tga_prob_cal
      ),
    join_by(game_id, play_id, nfl_id)
  ) %>%
  mutate(
    tga_prob_at_tackle = replace_na(tga_prob_at_tackle, 0)
  )

# calculate PPF 
val_pred_df <- val_pred_df %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    tackle_attempt_any = if_else(any(tackle_attempt == 1), 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    positioning = (AVG_TACKLE_PROB * ta_cond_prob_cal) - (AVG_TACKLE_PROB * ta_start_cond_prob_cal),
    pursuit = (tackle_attempt_any * tga_prob_at_tackle - AVG_TACKLE_PROB * ta_cond_prob_cal),
    finish = tackle_attempt_any * (tackle - tga_prob_at_tackle),
    ToE = tackle - (AVG_TACKLE_PROB * ta_start_cond_prob_cal)
  ) 

# filter evaluation frames only 
MIN_PROB <- 0.1
val_pred_df <- val_pred_df %>%
  filter(time_to_tackle > 0 | is.na(time_to_tackle)) %>%
  group_by(game_id, play_id, nfl_id) %>%
  filter(
    (max(ta_cond_prob_cal) > MIN_PROB | max(ta_start_cond_prob_cal) > MIN_PROB)
  ) %>%
  ungroup()

val_pred_df <- val_pred_df %>%
  arrange(game_id, play_id, nfl_id)

# play summary
ppf_play_summary <- val_pred_df %>%
  group_by(game_id, play_id, nfl_id) %>%
  summarise(
    positioning = mean(positioning),
    pursuit = mean(pursuit),
    finish = mean(finish),
    ToE = mean(ToE),
    tackle_attempt = first(tackle_attempt),
    tackle = first(tackle),
    .groups = "drop"
  )

ppf_player_summary <- ppf_play_summary %>%
  group_by(nfl_id) %>%
  summarise(
    n = n(),
    across(positioning:ToE, ~ mean(.x) * 100),
    across(c(tackle_attempt, tackle), mean)
  ) %>%
  arrange(desc(ToE))

ppf_player_summary <- ppf_player_summary %>%
  left_join(
    players_df %>%
      select(nfl_id, display_name, position),
    by = "nfl_id"
  )
write_csv(ppf_player_summary, "data/ppf_player_summary.csv")


