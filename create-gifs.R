library(magick)
library(tidyverse)
source("src/preprocess-data.R")
source("src/utils/report-animation.R")

# set multi processing strategy
future::plan(strategy = "cluster", workers = 5)

cum_mean <- function(x) {
  num_not_na <- cumsum(!is.na(x))
  x[is.na(x)] <- 0
  out <- cumsum(x) / num_not_na
  out[is.nan(out)] <- 0
  return(out)
}

tackle_df <- read_csv("data/tackles.csv", col_types = cols()) %>%
  janitor::clean_names()
plays_df <- read_csv("data/plays.csv", col_types = cols()) %>%
  janitor::clean_names()
tracking_df <- map_df(1:9, read_tracking_data) %>%
  janitor::clean_names()

id_df <- val_pred_df %>%
  distinct(game_id, play_id)

GAME_ID <- id_df$game_id[1]
PLAY_ID <- id_df$play_id[1]

tracking_df <- tracking_df %>%
  filter(
    game_id == GAME_ID,
    play_id == PLAY_ID
  )

# mirror tracking data, so all plays are in the same direction
tracking_df <- tracking_df %>%
  mutate(
    x = if_else(play_direction == "left", 120 - x, x),
    y = if_else(play_direction == "left", 160/3 - y, y),
    o = case_when(
      play_direction == "left" & o > 180 ~ o - 180,
      play_direction == "left" & o < 180 ~ o + 180,
      TRUE ~ o
    ),
    dir = case_when(
      play_direction == "left" & dir > 180 ~ dir - 180,
      play_direction == "left" & dir < 180 ~ dir + 180,
      TRUE ~ dir
    )
  )

# nest tracking data for each unique game and play IDs
tracking_df <- tracking_df %>%
  nest(.by = c("game_id", "play_id"))

tracking_df <- tracking_df %>%
  left_join(
    plays_df %>%
      select(
        game_id, 
        play_id, 
        ball_carrier_id, 
        los = absolute_yardline_number
      ),
    by = c("game_id", "play_id")
  )

# nest tackle data, so you can join it to the tracking data
tracking_df <- tracking_df %>%
  left_join(
    tackle_df %>%
      nest(.by = c("game_id", "play_id"), .key = "tackle_data"), 
    by = c("game_id", "play_id")
  )

# add tackle frame column to the nested tackle data based on min dist relative to bc
tracking_df <- tracking_df %>%
  mutate(
    tackle_data = future_pmap(
      list(data, tackle_data, ball_carrier_id),
      get_tackle_frames
    )
  )

play_df <- tracking_df$data[[1]] %>%
  filter_active_frames()
play_df <- play_df %>%
  left_join(
    val_pred_df %>%
      filter(game_id == GAME_ID, play_id == PLAY_ID) %>%
      mutate(tackle_prob = ta_cond_prob_cal * 0.87) %>%
      select(nfl_id, frame_id, tackle_prob, ta_start_cond_prob_cal),
    by = c("nfl_id", "frame_id")
  ) %>%
  filter(frame_id != max(frame_id))

play_details <- plays_df %>%
  filter(
    game_id == GAME_ID,
    play_id == PLAY_ID
  )
play_df$ball_carrier_id <- play_details$ball_carrier_id
play_df$offense_team <- play_details$possession_team

play_val_pred_df <- val_pred_df %>%
  filter(
    game_id == GAME_ID,
    play_id == PLAY_ID
  ) %>%
  arrange(nfl_id, frame_id) %>%
  group_by(nfl_id) %>%
  mutate(
    cume_position = cum_mean(positioning),
    cume_pursuit = cum_mean(pursuit),
    cume_ToE = cum_mean(ToE)
  ) %>%
  ungroup()

play_val_pred_df <- play_val_pred_df %>%
  left_join(
    play_df %>%
      select(nfl_id, display_name, jersey_number) %>%
      distinct_all(),
    by = "nfl_id"
  ) 

animate_report_play(play_df)
animate_report_metrics(play_val_pred_df)

gif1 <- image_read("visualizations/play.gif")
gif2 <- image_read("visualizations/metrics.gif")

gif_cmb <- image_append(c(gif1[1], gif2[1]))
for(i in 2:length(gif1)) {
  gif_cmb <- c(gif_cmb, image_append(c(gif1[i], gif2[i])))
}
gif_cmb
image_write_gif(gif_cmb, "visualizations/play_metric.gif")

animate_report_play_static(play_df)
animate_start_metric(play_val_pred_df)

gif1 <- image_read("visualizations/play_static.gif")
gif2 <- image_read("visualizations/start_prob.gif")

gif_cmb <- image_append(c(gif1[1], gif2[1]))
for(i in 2:length(gif1)) {
  gif_cmb <- c(gif_cmb, image_append(c(gif1[i], gif2[i])))
}
gif_cmb
image_write_gif(gif_cmb, "visualizations/play_start_prob.gif")


