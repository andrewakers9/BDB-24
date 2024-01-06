library(ggplot2)
library(gganimate)
library(ggnewscale)


animate_play <- function(play, direction_arrows = FALSE) {
  
  if(!is.data.frame(play)) play <- play[[1]]
  
  if("tackle_prob" %in% colnames(play)) {
    size_col <- "tackle_prob"
    size_scale <- scale_size_continuous(
      limits = c(0, 1),
      range = c(3, 10)
    )
    play <- play %>%
      group_by(nfl_id) %>%
      mutate(
        tackle_prob = case_when(
          club == "football" ~ 0.0,
          all(is.na(tackle_prob)) ~ 0.1,
          TRUE ~ tackle_prob
        )
      ) %>%
      ungroup()
  } else {
    size_col <- "club"
    size_scale <- scale_size_manual(values = c(6, 4, 6), guide = FALSE)
    
  }
  
  play <- play %>%
    mutate(
      x_next = x + (s * 1760 / 3600 * sin(deg_2_rad(dir))),
      y_next = y + (s * 1760 / 3600 * cos(deg_2_rad(dir)))
    ) 
  
  cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
  cols_col <- c("#000000", "#663300", "#000000")
  
  teams <- unique(play$club[play$club != "football"])
  play <- play %>%
    mutate(
      club = factor(club, levels = c(teams[1], "football", teams[2]))
    )
  
  play <- play %>%
    mutate(
      jersey_number = if_else(club == offense_team, NA, jersey_number)
    )
  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  #specific boundaries
  ymin <- max(round(min(play$x, na.rm = TRUE) - 20, -1), 0)
  ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
  num_yardlines <- length(seq(10, ymax, by = 10))
  
  #hash marks
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  p1 <- ggplot() +
    
    #setting size and color parameters
    size_scale + 
    scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
    scale_fill_manual(values = cols_fill, guide = FALSE) + 
    scale_colour_manual(values = cols_col, guide = FALSE) +
    
    #adding hash marks
    #annotate("text", x = df.hash$x[df.hash$x < 55/2], 
    #         y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    #annotate("text", x = df.hash$x[df.hash$x > 55/2], 
    #         y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
    
    #adding yard lines
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    
    #adding field yardline text
    annotate("text", x = rep(hash.left, length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G")[1:num_yardlines], 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")[1:num_yardlines], 
             angle = 90, size = 4) + 
    
    #adding field exterior
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    
    #adding players
    geom_point(data = play, aes(x = (xmax-y),
                                y = x, 
                                shape = club,
                                fill = club,
                                group = nfl_id,
                                size = !!sym(size_col),
                                colour = club) 
               ) +  
    
    #adding jersey numbers
    geom_text(data = play, aes(x = (xmax-y), y = x, label = jersey_number), colour = "black", 
              vjust = 0.36, size = 3.5)
  
  if(direction_arrows) {
    
      p1 <- p1 + 
        geom_segment(
          data = play,
          aes(x = (xmax - y), y = x, xend = (xmax - y_next), yend = x_next),
          arrow = arrow(length = unit(0.1, "in")) 
      ) 
      
  }
    
    
  p1 +
    
    #applying plot limits
    ylim(ymin, ymax) + 
    coord_fixed() +
    
    #theme
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) +
    
    #setting animation parameters
    transition_time(frame_id)  +
    ease_aes('linear') + 
    NULL
    
}

animate_report_play <- function(play, direction_arrows = FALSE) {
  
  players <- c(44829, 47956, 53448)
  
  if(!is.data.frame(play)) play <- play[[1]]
  
  if("tackle_prob" %in% colnames(play)) {
    size_col <- "tackle_prob"
    size_scale <- scale_size_continuous(
      name = "Tackle Prob",
      limits = c(0, 1),
      range = c(4, 10)
    )
    play <- play %>%
      group_by(nfl_id) %>%
      mutate(
        tackle_prob = case_when(
          club == "football" ~ 0.0,
          club == offense_team ~ 0.1,
          TRUE ~ tackle_prob
        )
      ) %>%
      ungroup()
  } else {
    size_col <- "club"
    size_scale <- scale_size_manual(values = c(6, 4, 6), guide = FALSE)
    
  }
  
  play <- play %>%
    mutate(
      x_next = x + (s * 1760 / 3600 * sin(deg_2_rad(dir))),
      y_next = y + (s * 1760 / 3600 * cos(deg_2_rad(dir)))
    ) 
  
  # id colors for highlighted players
  play <- play %>%
    mutate(
      custom_fill = case_when(
        display_name == "football" ~ "#663300",
        nfl_id == 44829 ~ "#7fea93",
        nfl_id == 47956 ~ "#8de2f9",
        nfl_id == 53448 ~ "#e74cf1",
        club == offense_team ~ "#c83803",
        TRUE ~ "#5A1414"
      )
    )
  
  play <- play %>%
    mutate(
      jersey_number = if_else(!(nfl_id %in% players), NA, jersey_number)
    )
  
  cols_col <- c("#000000", "#663300", "#000000")
  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  #specific boundaries
  ymin <- max(round(min(play$x, na.rm = TRUE) - 20, -1), 0)
  ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
  
  num_yardlines <- length(seq(10, ymax, by = 10))
  
  #hash marks
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  p1 <- ggplot() +
    
    #setting size and color parameters
    size_scale + 
    scale_fill_identity(guide = FALSE) +
    scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
    scale_colour_manual(values = cols_col, guide = FALSE) +
    
    #adding hash marks
    #annotate("text", x = df.hash$x[df.hash$x < 55/2], 
    #         y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    #annotate("text", x = df.hash$x[df.hash$x > 55/2], 
    #         y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
    
    #adding yard lines
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    
    #adding field yardline text
    annotate("text", x = rep(hash.left, length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G")[1:num_yardlines], 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")[1:num_yardlines], 
             angle = 90, size = 4) + 
    
    #adding field exterior
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    
    #adding players
    geom_point(data = play, aes(x = (xmax-y),
                                y = x, 
                                shape = club,
                                fill = custom_fill,
                                group = nfl_id,
                                size = !!sym(size_col),
                                colour = club) 
    ) +  
    
    #adding jersey numbers
    geom_text(data = play, aes(x = (xmax-y), y = x, label = jersey_number), colour = "black", 
              vjust = 0.36, size = 3.5)
  
  if(direction_arrows) {
    
    p1 <- p1 + 
      geom_segment(
        data = play,
        aes(x = (xmax - y), y = x, xend = (xmax - y_next), yend = x_next),
        arrow = arrow(length = unit(0.1, "in")) 
      ) 
    
  }
  
  
  p1 <- p1 +
    
    #applying plot limits
    #ylim(ymin, ymax) + 
    coord_fixed() +
    
    #theme
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) +
    
    #setting animation parameters
    transition_time(frame_id)  +
    ease_aes('linear') + 
    NULL
  
  gif <- animate(p1,
                 fps = 10,
                 duration = 15,
                 width = 500, height = 500,
                 renderer = gifski_renderer("visualizations/play.gif"))
  
}

animate_report_metrics <- function(df) {

  players <- c(44829, 47956, 53448)
  metric_order <- c("Position", "Pursuit", "Finish", "ToE")
  
  df <- play_val_pred_df %>%
    filter(nfl_id %in% players) %>%
    select(nfl_id, 
           display_name, 
           jersey_number,
           frame_id, 
           positioning:ToE)
    
  df <- df %>%
    group_by(nfl_id) %>% 
    mutate(
      Time = row_number() 
    ) %>%
    fill(positioning:ToE, .direction = "down") %>%
    ungroup()
  
  df <- df %>%
    select(-frame_id) %>%
    pivot_longer(
      -c(nfl_id, display_name, jersey_number, Time),
      names_to = "metrics",
      values_to = "value"
    ) %>%
    mutate(
      metrics = case_when(
        metrics == "positioning" ~ "Position",
        metrics == "pursuit" ~ "Pursuit",
        metrics == "finish" ~ "Finish",
        TRUE ~ "ToE"
      ),
      metrics = factor(metrics, levels = metric_order),
      val_labels = as.character(round(value, 2))
    ) 
  df <- df %>%
    mutate(
      legend_name = str_c(display_name, " (", jersey_number, ")")
    )
  p <- df %>%
    ggplot(
      aes(Time, value, color = legend_name, group = legend_name)
    ) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_point(size = 2) + 
    geom_line(linewidth = 0.8) +
    facet_wrap(~ metrics) +
    xlab("") +
    ylab("") +
    scale_color_manual(
      values = c("#8de2f9", "#e74cf1", "#7fea93"),
      name = NULL
    ) +
    theme(
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_blank()
    ) +
    transition_reveal(Time)

  gif <- animate(p,
                 fps = 10,
                 duration = 15,
                 width = 500, height = 500,
                 renderer = gifski_renderer("visualizations/metrics.gif"))
  
}

animate_report_play_static <- function(play) {
  
  players <- c(44829, 47956, 53448)
  
  if(!is.data.frame(play)) play <- play[[1]]
  
  play <- play %>%
    select(-tackle_prob) %>%
    rename(tackle_prob = ta_start_cond_prob_cal) %>%
    mutate(ghost = FALSE)
  
  # create ghost defenders
  play <- play %>%
    bind_rows(
      play %>%
        filter(nfl_id %in% players) %>%
        mutate(ghost = TRUE)
    )
  
  # freeze defenders 
  play <- play %>%
    group_by(nfl_id) %>%
    mutate(
      x = if_else(club != "football" & club != offense_team & !ghost, first(x), x),
      y = if_else(club != "football" & club != offense_team & !ghost, first(y), y)
    ) %>%
    ungroup()
  
  if("tackle_prob" %in% colnames(play)) {
    size_col <- "tackle_prob"
    size_scale <- scale_size_continuous(
      name = "Tackle Prob",
      limits = c(0, 0.5),
      range = c(4, 10)
    )
    play <- play %>%
      group_by(nfl_id) %>%
      mutate(
        tackle_prob = case_when(
          club == "football" ~ 0.0,
          club == offense_team | ghost ~ 0.1,
          TRUE ~ tackle_prob
        )
      ) %>%
      ungroup()
  } else {
    size_col <- "club"
    size_scale <- scale_size_manual(values = c(6, 4, 6), guide = FALSE)
    
  }
  
  play <- play %>%
    mutate(
      x_next = x + (s * 1760 / 3600 * sin(deg_2_rad(dir))),
      y_next = y + (s * 1760 / 3600 * cos(deg_2_rad(dir)))
    ) 
  
  # id colors for highlighted players
  play <- play %>%
    mutate(
      custom_fill = case_when(
        display_name == "football" ~ "#663300",
        ghost ~ "grey",
        nfl_id == 44829 ~ "#7fea93",
        nfl_id == 47956 ~ "#8de2f9",
        nfl_id == 53448 ~ "#e74cf1",
        club == offense_team ~ "#c83803",
        TRUE ~ "#5A1414"
      )
    )
  
  play <- play %>%
    mutate(
      jersey_number = if_else(!(nfl_id %in% players), NA, jersey_number)
    )
  
  bc_df <- play %>%
    filter(nfl_id == ball_carrier_id)
  
  cols_col <- c("#000000", "#663300", "#000000")
  
  # General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  #specific boundaries
  ymin <- max(round(min(play$x, na.rm = TRUE) - 20, -1), 0)
  ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
  
  num_yardlines <- length(seq(10, ymax, by = 10))
  
  #hash marks
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  p1 <- ggplot() +
    
    #setting size and color parameters
    size_scale + 
    scale_fill_identity(guide = FALSE) +
    scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
    scale_colour_manual(values = cols_col, guide = FALSE) +
    
    #adding hash marks
    #annotate("text", x = df.hash$x[df.hash$x < 55/2], 
    #         y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    #annotate("text", x = df.hash$x[df.hash$x > 55/2], 
    #         y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
    
    #adding yard lines
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    
    #adding field yardline text
    annotate("text", x = rep(hash.left, length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G")[1:num_yardlines], 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), length(seq(10, ymax, by = 10))), y = seq(10, ymax, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")[1:num_yardlines], 
             angle = 90, size = 4) + 
    
    #adding field exterior
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    
    #adding ball carrier path 
    annotate("line", x = xmax - bc_df$y, y = bc_df$x, color = "gold", linewidth = 1.2) +
    
    #adding players
    geom_point(data = play, aes(x = (xmax-y),
                                y = x, 
                                shape = club,
                                fill = custom_fill,
                                group = nfl_id,
                                size = !!sym(size_col),
                                colour = club) 
    ) +  
    
    #adding jersey numbers
    geom_text(data = play, aes(x = (xmax-y), y = x, label = jersey_number), colour = "black", 
              vjust = 0.36, size = 3.5) 
  
  
  p1 <- p1 +
    
    #applying plot limits
    #ylim(ymin, ymax) + 
    coord_fixed() +
    
    #theme
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) +
    
    #setting animation parameters
    transition_time(frame_id)  +
    ease_aes('linear') + 
    NULL
  
  gif <- animate(p1,
                 fps = 10,
                 duration = 15,
                 width = 500, height = 500,
                 renderer = gifski_renderer("visualizations/play_static.gif"))
  
}

animate_start_metric <- function(df) {
  
  players <- c(44829, 47956, 53448)
  df <- df %>%
    filter(nfl_id %in% players) %>%
    group_by(nfl_id) %>%
    mutate(Time = row_number()) %>%
    ungroup()
  
  df <- df %>%
    rename(prob = ta_start_cond_prob_cal) %>%
    mutate(
      prob_label = as.character(round(prob, 2)),
      legend_name = str_c(display_name, " (", jersey_number, ")")
    )
  
  p <- df %>%
    ggplot(
      aes(Time, prob, color = legend_name, group = legend_name)
    ) +
    geom_line(linewidth = 0.8) +
    geom_segment(aes(xend = max(Time), yend = prob),
                 color = "grey",
                 linetype = 2,
                 show.legend = FALSE) +
    geom_point(size = 2) + 
    geom_text(aes(x = max(Time), y = prob, label = prob_label),
              color = "black",
              hjust = 0) +
    coord_cartesian(clip = "off") + 
    scale_color_manual(
      values = c("#8de2f9", "#e74cf1", "#7fea93"),
      name = NULL
    ) +
    xlab("") +
    scale_y_continuous(
      name = "Tackle Prob"
    ) +
    theme(
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_blank()
    ) +
    transition_reveal(Time)
    
  gif <- animate(p,
                 fps = 10,
                 duration = 15,
                 width = 500, height = 500,
                 renderer = gifski_renderer("visualizations/start_prob.gif"))
  
}



