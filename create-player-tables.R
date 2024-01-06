library(nflreadr)
library(magick)
library(gtExtras)
library(gt)

ppf_player_summary <- read_csv("data/ppf_player_summary.csv", col_types = cols())
player_images <- nflreadr::load_players() %>%
  select(nfl_id = gsis_it_id, team_abbr, headshot)
team_logos <- nflreadr::load_teams() %>%
  select(team_abbr, team_logo_wikipedia)

ppf_player_summary <- ppf_player_summary %>%
  filter(n >= 25) %>%
  arrange(desc(ToE)) %>%
  left_join(
    player_images, by = "nfl_id"
  ) %>%
  left_join(
    team_logos, by = "team_abbr"
  )

player_summary_table <- function(df,
                                 position = c("Linebackers", "Defensive Lineman", "Defensive Backs"),
                                 n_players = 20,
                                 save_fp = NULL) {
  
  table_title <- md(glue::glue("**Top {n_players} {position}**"))
  if(position == "Linebackers") {
    position_codes <- c("OLB", "ILB", "MLB")
  } else if(position == "Defensive Backs") {
    position_codes <- c("DB", "CB", "SS", "FS")
  } else {
    position_codes <- c("DT", "DE", "NT")
  }

  gt_table <- df %>%
    filter(position %in% position_codes) %>%
    slice_head(n = 20) %>% 
    select(
      display_name,
      headshot,
      team_logo_wikipedia,
      plays = n,
      positioning:ToE
    ) %>%
    gt(rowname_col = "display_name") %>%
    tab_header(
      title = table_title,
      subtitle = md("Average PPF ToE per 100 Plays | *2022 Season Weeks 6-9 | Min. 25 Evaluation Plays*")
    ) %>%
    tab_stubhead(label = "Player") %>%
    gt_img_rows(headshot, height = 25) %>%
    gt_img_rows(team_logo_wikipedia, height = 25) %>%
    fmt_number(positioning:ToE, decimals = 1) %>%
    data_color( 
      columns = ToE, 
      fn = scales::col_numeric( 
        palette = c('white', 'orange', 'red'),
        domain = c(0, 30) 
      )
    ) %>%
    cols_label(
      headshot = "",
      team_logo_wikipedia = "Team",
      plays = "Plays",
      positioning = "Position",
      pursuit = "Pursuit",
      finish = "Finish"
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = where(is.numeric))
    )
  
  if(!is_null(save_fp)) {
    gtsave(gt_table, save_fp, vwidth = 800, vheight = 1500)
  }
  
  gt_table
  
}

player_summary_table(ppf_player_summary, 
                     position = "Linebackers",
                     save_fp = "visualizations/linebacker_table.PNG")
player_summary_table(ppf_player_summary,
                     position = "Defensive Backs",
                     save_fp = "visualizations/db_table.PNG")
player_summary_table(ppf_player_summary,
                     position = "Defensive Lineman",
                     save_fp = "visualizations/lineman_table.PNG")

table1 <- image_read("visualizations/linebacker_table.PNG")
table2 <- image_read("visualizations/db_table.PNG")
table3 <- image_read("visualizations/lineman_table.PNG")

table_cmb <- image_append(c(table1, table2))
table_cmb <- image_append(c(table_cmb, table3), stack = TRUE)
image_write(table_cmb, "visualizations/cmb_table.PNG")
