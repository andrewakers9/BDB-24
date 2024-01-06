
# helper functions
deg_2_rad <- function(deg) {
  rad <- deg * pi /180
  return(rad)
}

angle_rotate <- function(a, rotation) {
  a <- a + rotation
  a <- ifelse(a < 0, a + 360,
              ifelse(a > 360, a - 360, a))
  return(a)
}

angle_diff <- function(a, b) {
  a <- a - b
  a <- (a + 180) %% 360 - 180
  return(a)
}

read_tracking_data <- function(week_num, dir = "data/") {
  file_name <- paste0(dir, "tracking_week_", week_num, ".csv")
  df <- read_csv(file_name, col_types = cols())
  return(df)
}

get_last_event <- function(data) {
  out <- data %>%
    filter(!is.na(event)) %>%
    distinct(frame_id, .keep_all = TRUE) %>%
    pull(event) %>%
    last()
  return(out)
}

get_tackle_frames <- function(play_df,
                              play_tackle_df,
                              ball_carrier_id) {
  
  if(is_null(play_tackle_df)) return(NULL)
  if(all(play_df$display_name == "football")) return(NULL)
  
  play_tackle_df <- play_tackle_df %>%
    filter(
      (tackle == 1 | assist == 1 | pff_missed_tackle == 1)
    )
  
  play_df <- filter_active_frames(play_df)
  
  if(is_null(play_df)) return(NULL)
  if(nrow(play_df) == 0) return(NULL)
  
  if(nrow(play_tackle_df) > 0) {
    
    bc_data <- play_df %>%
      filter(nfl_id == ball_carrier_id) %>%
      select(frame_id, bc_x = x, bc_y = y)
    
    ids <- play_tackle_df %>%
      pull(nfl_id)
    
    play_df_filt <- play_df %>%
      filter(nfl_id %in% ids) %>%
      select(frame_id, nfl_id, x, y) %>%
      left_join(bc_data, by = "frame_id")
    
    # calculate distance between ball carrier and player(s) that missed tackle
    play_df_filt <- play_df_filt %>%
      mutate(
        dist = sqrt((x - bc_x)^2 + (y - bc_y)^2)
      )
    
    # assume tackle occurred at the minimum dist between ball carrier and tackler
    if(length(ids) > 1) {
      tackle_frame_df <- play_df_filt %>%
        group_by(nfl_id) %>%
        filter(dist == min(dist)) %>% 
        ungroup() %>%
        distinct(nfl_id, .keep_all = TRUE)
      
      play_tackle_df <- play_tackle_df %>%
        left_join(
          tackle_frame_df %>%
            select(nfl_id, tackle_frame = frame_id), 
          by = "nfl_id"
        )
      
    } else {
      
      join_df <- data.frame(
        nfl_id = ids[1],
        tackle_frame = play_df_filt$frame_id[which.min(play_df_filt$dist)]
      )
      
      play_tackle_df <- play_tackle_df %>%
        left_join(join_df, by = "nfl_id")
      
    }
    
  }
  
  return(play_tackle_df)
  
}

filter_active_frames <- function(play_df, 
                                 play_tackle_df = NULL) {
  
  start_events <- c(
    "pass_outcome_caught",
    "pass_arrived",
    "handoff",
    "snap_direct",
    "run"
  )
  end_events <- c(
    "tackle",
    "out_of_bounds",
    "safety",
    "touchdown",
    "autoevent_passforward",
    "qb_slide",
    "qb_sack",
    "fumble"
  )
  
  play_events <- play_df %>%
    filter(!is.na(event)) %>%
    select(frame_id, event) %>%
    distinct_all() 
  
  if(!any(play_events$event %in% start_events)) return(NULL)
  
  start_frame <- play_events$frame_id[which(play_events$event %in% start_events)][1]
  end_frame <- play_events$frame_id[which(play_events$event %in% end_events)][1]
  
  # make adjustments if end frame occurs after tackler min point relative to the ball carrier
  if(!is_null(play_tackle_df) & any(play_tackle_df$tackle == 1 | play_tackle_df$assist == 1)) {
    end_frame <- play_tackle_df %>%
      filter(tackle == 1 | assist == 1) %>%
      pull(tackle_frame) %>%
      max()
  }
  
  play_df <- play_df %>%
    filter(
      between(frame_id, start_frame, end_frame)
    )
  
  return(play_df)
  
}

get_2d_inputs <- function(ball_carrier_id, 
                          LOS,
                          pass_play,
                          play_df, 
                          play_tackle_df,
                          every_n_frames = 1) {
  
  play_df <- filter_active_frames(play_df, play_tackle_df)
  
  if(is_null(play_df)) return(NULL)
  
  frames <- unique(play_df$frame_id)
  if(every_n_frames > 1) {
    frames <- seq(frames[1], frames[length(frames)], by = every_n_frames)
  }
  
  # ensure that tackle frames are included 
  if(!is_null(play_tackle_df) & every_n_frames > 1) {
    tackle_frames <- play_tackle_df$tackle_frame
    frames <- c(frames, tackle_frames)
  }
  
  # get x position relative to LOS 
  play_df <- play_df %>%
    mutate(
      rel_los_x = if_else(play_direction == "left", x - (120 - LOS), x - LOS),
      pass_play = pass_play
    )
  
  play_df <- play_df %>%
    filter(frame_id %in% frames) %>%
    select(
      frame_id,
      nfl_id,
      club,
      pass_play,
      x, y, rel_los_x, s, acc, dir
    )
  
  # project speed to x and y axes 
  play_df <- play_df %>%
    mutate(
      speed_x = sin(deg_2_rad(dir)) * s,
      speed_y = cos(deg_2_rad(dir)) * s
    )
  
  ball_carrier_df <- play_df %>%
    filter(nfl_id == ball_carrier_id) %>%
    mutate(
      yards_to_goal = 110 - x,
      yards_from_sideline = pmin(53.333 - y, y)
    )
  
  offense_team <- ball_carrier_df %>%
    slice(1) %>%
    pull(club)
  
  # join ball carrier data to get basic relative statistics 
  defense_df <- play_df %>%
    filter(
      club != "football",
      club != offense_team
    ) %>%
    left_join(
      ball_carrier_df %>%
        select(
          nfl_id,
          frame_id,
          x, y, 
          speed_x, speed_y, acc,
          yards_to_goal
        ),
      by = "frame_id",
      suffix = c("", "_bc")
    )
  
  defense_df <- defense_df %>%
    mutate(
      rel_x = x - x_bc,
      rel_y = y - y_bc,
      dist = sqrt(rel_x^2 + rel_y^2), 
      rel_s_x = speed_x - speed_x_bc,
      rel_s_y = speed_y - speed_y_bc
    )
  
  # add frame id adjusted for when the play starts 
  defense_df <- defense_df %>%
    group_by(nfl_id) %>%
    mutate(
      frame_id_adj = row_number()
    ) %>%
    ungroup()
  
  # get coordinates for the closest defender relative to the ball carrier 
  closest_defender <- defense_df %>%
    arrange(frame_id, dist) %>%
    distinct(frame_id, .keep_all = TRUE) %>%
    select(
      frame_id,
      rel_x_d1 = rel_x,
      rel_y_d1 = rel_y,
      rel_s_x_d1 = rel_s_x,
      rel_s_y_d1 = rel_s_y
    )
  
  defense_df <- defense_df %>%
    left_join(closest_defender, 
              by = "frame_id",
              relationship = "many-to-one")
  
  defense_df <- defense_df %>%
    arrange(frame_id, dist) %>%
    group_by(frame_id) %>%
    mutate(proximity_rank = row_number()) %>%
    ungroup()
  
  # get features relative to defender starting point 
  defense_df <- defense_df %>%
    group_by(nfl_id) %>%
    mutate(
      rel_x_start = first(rel_x),
      rel_y_start = first(rel_y),
      proximity_rank_start = first(proximity_rank),
      rel_s_x_start = first(rel_s_x),
      rel_s_y_start = first(rel_s_y)
    ) %>%
    ungroup()
  
  # to get stats for the closest defender relative to each other defender 
  defense_df <- defense_df %>%
    left_join(
      defense_df %>%
        select(
          frame_id,
          nfl_id_join = nfl_id,
          x_d = x,
          y_d = y,
          rel_x_d = rel_x,
          rel_y_d = rel_y,
          rel_s_x_d = rel_s_x,
          rel_s_y_d = rel_s_y
        ),
      by = "frame_id",
      relationship = "many-to-many"
    ) %>%
    filter(
      nfl_id != nfl_id_join
    ) %>%
    mutate(
      dist_d = sqrt((x - x_d)^2 + (y - y_d)^2)
    ) %>%
    arrange(
      frame_id, nfl_id, dist_d
    ) %>%
    distinct(frame_id, nfl_id, .keep_all = TRUE) %>%
    select(
      -c(nfl_id_join, x_d, y_d, dist_d)
    )
  
  # to get distance of each defender to the closest offensive player 
  offense_df <- play_df %>%
    filter(
      club == offense_team,
      nfl_id != ball_carrier_id
    ) %>%
    select(
      frame_id, 
      nfl_id, 
      x_off = x, 
      y_off = y,
      speed_x_off = speed_x,
      speed_y_off = speed_y
    )
  
  defense_df <- defense_df %>%
    left_join(
      offense_df %>%
        select(
          frame_id,
          x_off, 
          y_off,
          speed_x_off,
          speed_y_off
        ),
      by = "frame_id",
      relationship = "many-to-many"
    ) %>%
    mutate(
      dist_to_off = sqrt((x - x_off)^2 + (y - y_off)^2)
    ) %>%
    arrange(frame_id, nfl_id, dist_to_off) %>%
    distinct(frame_id, nfl_id, .keep_all = TRUE)
  
  defense_df <- defense_df %>%
    mutate(
      rel_x_off = x - x_off,
      rel_y_off = y - y_off,
      rel_s_x_off = speed_x - speed_x_off,
      rel_s_y_off = speed_y - speed_y_off
    ) %>%
    select(
      -c(x_off, 
         y_off,
         speed_x_off,
         speed_y_off)
    ) %>%
    arrange(frame_id, proximity_rank)
  
  # for plays with no tackle and two assists, give both players tackle credit 
  if(sum(play_tackle_df$tackle) == 0 & sum(play_tackle_df$assist) > 1) {
    play_tackle_df$tackle[which(play_tackle_df$assist == 1)] <- 1
  }
  
  if(is_null(play_tackle_df)) {
    # add placeholder columns
    defense_df <- defense_df %>%
      mutate(
        tackle = 0,
        pff_missed_tackle = 0,
        tackle_frame = NA_integer_
      )
  } else {
    defense_df <- defense_df %>%
      left_join(
        play_tackle_df %>%
          filter(tackle == 1 | pff_missed_tackle == 1),
        by = "nfl_id"
      )
  }
  
  defense_df <- defense_df %>%
    mutate(
      tackle = replace_na(tackle, 0),
      pff_missed_tackle = replace_na(pff_missed_tackle, 0),
      tackle_attempt = if_else(
        pff_missed_tackle == 1 & frame_id <= tackle_frame, 1, tackle
      )
    )
  
  # add time to end of play for benchmark model 
  defense_df <- defense_df %>%
    mutate(
      time_to_end = max(frame_id) - frame_id
    )
  
  defense_df <- defense_df %>%
    select(
      frame_id,
      frame_id_adj,
      nfl_id,
      nfl_id_bc,
      pass_play,
      dist,
      rel_los_x,
      rel_x, rel_y,
      speed_x, speed_y, acc,
      rel_s_x, rel_s_y,
      proximity_rank,
      rel_x_d1, rel_y_d1,
      rel_s_x_d1, rel_s_y_d1,
      rel_x_d, rel_y_d,
      rel_s_x_d, rel_s_y_d,
      rel_x_off, rel_y_off,
      rel_s_x_off, rel_s_y_off,
      speed_x_bc,
      speed_y_bc,
      acc_bc,
      yards_to_goal,
      y_bc,
      time_to_end,
      rel_x_start, rel_y_start,
      proximity_rank_start,
      rel_s_x_start, rel_s_y_start,
      tackle_frame,
      tackle,
      tackle_attempt
    )
  
  return(defense_df)
  
}

## main preprocess function
preprocess <- function() {
  
  # read data 
  games_df <- read_csv("data/games.csv", col_types = cols()) %>%
    janitor::clean_names()
  players_df <- read_csv("data/players.csv", col_types = cols()) %>%
    janitor::clean_names()
  plays_df <- read_csv("data/plays.csv", col_types = cols()) %>%
    janitor::clean_names()
  tackle_df <- read_csv("data/tackles.csv", col_types = cols()) %>%
    janitor::clean_names()
  tracking_df <- map_df(1:9, read_tracking_data) %>%
    janitor::clean_names()
  
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
  
  # add acceleration to tracking data before nesting
  tracking_df <- tracking_df %>%
    group_by(game_id, play_id, nfl_id) %>%
    mutate(
      acc = (s - lag(s)) / 0.1
    ) %>%
    ungroup()
  
  # nest tracking data for each unique game and play IDs
  tracking_df <- tracking_df %>%
    nest(.by = c("game_id", "play_id"))
  
  tracking_df <- tracking_df %>%
    arrange(game_id, play_id)
  
  plays_df <- plays_df %>%
    arrange(game_id, play_id) %>% 
    mutate(
      pass_play = if_else(is.na(pass_result), 1, 0)
    )
  
  tracking_df <- tracking_df %>%
    left_join(
      plays_df %>%
        select(
          game_id, 
          play_id, 
          ball_carrier_id, 
          pass_play,
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
  
  # get model inputs
  inputs <- future_pmap(
    list(
      ball_carrier_id = tracking_df$ball_carrier_id,
      pass_play = tracking_df$pass_play,
      LOS = tracking_df$los,
      play_df = tracking_df$data,
      play_tackle_df = tracking_df$tackle_data,
      every_n_frames = 1
    ),
    safely(get_2d_inputs)
  )
  
  # filter out errors and NAs (only occurs for a handful of plays)
  input_filter <- map_lgl(
    inputs,
    ~ is.data.frame(.x$result)
  )
  
  inputs <- inputs[input_filter]
  
  # extract rows per observation to be able to index feature rows by game/play
  rows_per_play <- map_dbl(inputs, ~ nrow(.x$result))
  
  # use weeks 1-5 for training data 
  model_idx_df <- plays_df %>%
    select(game_id, play_id) %>%
    filter(input_filter) %>%
    left_join(
      games_df %>% 
        select(game_id, week),
      by = "game_id"
    ) %>%
    mutate(idx = row_number())
  
  train_idx <- model_idx_df %>%
    filter(week <= 5)
  train_df <- map_df(inputs[train_idx$idx], ~.x$result)
  train_df <- train_df %>%
    mutate(
      time_to_tackle = tackle_frame - frame_id
    )
  
  # use weeks 6-7 for validation 
  val_idx <- model_idx_df %>%
    filter(week >= 6) 
  val_df <- map_df(inputs[val_idx$idx], ~ .x$result) %>%
    mutate(
      time_to_tackle = tackle_frame - frame_id
    )
  
  # impute missing acc data when play starts at the first frame
  med_acc <- median(train_df$acc, na.rm = TRUE)
  med_acc_bc <- median(train_df$acc_bc, na.rm = TRUE)
  train_df <- train_df %>%
    mutate(
      acc = replace_na(acc, med_acc),
      acc_bc = replace_na(acc_bc, med_acc_bc)
    )
  val_df <- val_df %>%
    mutate(
      acc = replace_na(acc, med_acc),
      acc_bc = replace_na(acc_bc, med_acc_bc)
    )
  
  # add game and play IDs to val set 
  val_df <- val_df %>%
    mutate(
      game_id = rep(val_idx$game_id, times = rows_per_play[val_idx$idx]),
      play_id = rep(val_idx$play_id, times = rows_per_play[val_idx$idx])
    ) %>%
    relocate(
      game_id, play_id
    )
  
  if(!dir.exists("data/")) dir.create("data/")
  
  write_csv(train_df, "data/train_df.csv")
  write_csv(val_df, "data/val_df.csv")
  
  return(
    invisible(NULL)
  )
  
}

