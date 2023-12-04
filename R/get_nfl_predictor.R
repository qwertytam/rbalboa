get_nfl_predictor <- function(game_id) {
  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")
  raw_get <- httr::GET(game_url, query = list(event = game_id, enable = "ranks,odds,linescores,logos"))
  
  httr::stop_for_status(raw_get)
  
  raw_json <- httr::content(raw_get)
  
  team_df <- raw_json[["boxscore"]][["teams"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unnest_wider(team) %>%
    select(id, name:displayName, logo) %>%
    mutate(location = c("away", "home")) %>%
    rename(
      team_name = name, team_full = displayName,
      team_abb = abbreviation, team_logo = logo
    )
  odds_df <-
    tibble(
      location = c("home", "away"),
      win_proj_fpi = c(
        as.double(raw_json$predictor$homeTeam$gameProjection),
        as.double(raw_json$predictor$awayTeam$gameProjection)
      ),
      team_id = c(
        raw_json$predictor$homeTeam$id,
        raw_json$predictor$awayTeam$id
      )
    ) %>%
    left_join(team_df, by = c("team_id" = "id")) %>%
    mutate(
      game_id = game_id,
      win_proj_fpi = as.double(win_proj_fpi)
    )
  
  odds_df
}