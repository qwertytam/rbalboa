get_season_dates <- function(){
  season_day_end <- "01"
  season_month_end <- "03"
  
  start_year <- substr(Sys.Date(), 1,4)
  start_month <- substr(Sys.Date(), 6,7)
  start_day <- substr(Sys.Date(), 9,10)
  
  if (as.integer(start_month) < as.integer(season_month_end)) {
    end_year <- as.integer(start_year)
  } else {
    end_year <- as.integer(start_year) + 1
  }
  
  season <- glue::glue(
    "{start_year}{start_month}{start_day}",
    "-{end_year}{season_month_end}{season_day_end}")
}


get_schedule <- function(season){
  
  message(glue::glue("Returning data for {season}!"))
  
  max_year <- substr(Sys.Date(), 1,4)
  
  if(!(as.integer(substr(season, 1, 4)) %in% c(1969:max_year))){
    message(paste("Error: Season must be between 1969 and", max_year))
  }
  
  # year > 1969
  season <- as.character(season)
  if(nchar(season) > 4){
    season_dates <- season
  } else {
    season_dates <- glue::glue("{season}0101-{season}1231")
  }
  
  raw_url <- "http://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"
  
  raw_get <- httr::GET(
    raw_url,
    query = list(
      limit = 1000,
      dates = season_dates
    )
  )
  
  httr::stop_for_status(raw_get)
  
  raw_sched <- httr::content(raw_get)
  
  sched_data <- raw_sched[["events"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unnest_wider(week) %>%
    rename(week = number) %>%
    unchop(competitions) %>%
    select(-id, -uid, -date, -status) %>%
    unnest_wider(competitions) %>%
    rename(matchup = name,
           matchup_short = shortName,
           game_id = id,
           game_uid = uid,
           game_date = date) %>%
    hoist(status,
          status_name = list("type", "name")) %>%
    select(!any_of(c("timeValid",
                     "neutralSite",
                     "conferenceCompetition",
                     "recent",
                     "type"))) %>%
    unnest_wider(season) %>%
    rename(season = year) %>%
    select(-any_of("status")) %>%
    hoist(
      competitors,
      home_team_name = list(1, "team", "name"),
      home_team_abb = list(1, "team", "abbreviation"),
      home_team_id = list(1, "team", "id"),
      home_team_location = list(1, "team", "location"),
      home_team_full = list(1, "team", "displayName"),
      home_score = list(1, "score"),
      home_win = list(1, "winner"),
      # away team
      away_team_name = list(2, "team", "name"),
      away_team_abb = list(2, "team", "abbreviation"),
      away_team_id = list(2, "team", "id"),
      away_team_location = list(2, "team", "location"),
      away_team_full = list(2, "team", "displayName"),
      away_score = list(2, "score"),
      away_win = list(2, "winner"),
    ) %>%
    mutate(home_win = as.integer(home_win),
           away_win = as.integer(away_win),
           home_score = as.integer(home_score),
           away_score = as.integer(away_score))
  
  sched_data %>% select(!where(is.list))
  
}