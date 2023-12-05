get_table_title <- function(team_selection){
  start_wk <- min(team_selection$week)
  end_wk <- max(team_selection$week)
  
  if (start_wk == end_wk){
    title <- str_glue("NFL Game Odds for Week {start_wk}")  
  } else {
    title <- str_glue("NFL Game Odds for Weeks {start_wk} to {end_wk}") 
  }
  
  title
}

make_table <- function(team_selection, time_now){
  tbl_title <- get_table_title(team_selection)

  gt_tbl <- team_selection %>%
    gt() %>%
    gt_theme_pff() %>%
    gt_color_box(
      columns = win_proj_fpi,
      domain = c(5, 95), width = 50, accuracy = 0.1,
      palette = "pff"
    ) %>%
    tab_header(
      title = tbl_title,
      subtitle = str_glue("Last updated on",
                          " {format(ud_time, '%a, %e %b %Y at %I:%M %p')}",
                          ud_time = with_tz(time_now, "US/Eastern")
      )
    ) %>%
    fmt_datetime(
      columns = date,
      date_style = "yMMMEd",
      time_style = "hm",
      locale = "en-AU"
    ) %>%
    cols_label(
      team_full = "Team",
      location = "Home/Away",
      win_proj_fpi = "Win Prob %"
    ) %>%
    tab_style(
      style = list(
        cell_borders("bottom", "white"),
        cell_fill(color = "#393c40")
      ),
      locations = cells_column_labels(win_proj_fpi)
    ) %>%
    tab_options(
      heading.title.font.size = "200%",
      heading.title.font.weight = "bolder"
    )
}