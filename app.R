# Install Required Packages ----------------------------------------------------
# renv::install(
#   packages = c(
#     "usethis",
#     "here",
#     "tidyverse",
#     # "googlesheets4",
#     "httpuv",
#     # "gt",
#     "gtExtras",
#     "shiny",
#     "packrat",
#     "rsconnect",
#     "testthat",
#     "styler",
#    "qwertytam/espnscrapeR"
#  ),
#  rebuild = TRUE
# )

# Script Set Up  ---------------------------------------------------------------
# Set up here
here::i_am("app.R")

# Load required packages
library(here)
library(tidyverse)
library(googlesheets4)
library(gt)
library(gtExtras)
library(shiny)
library(espnscrapeR)

# Set up authentication for use in Shiny environment
secrets_dir <- ".secrets"
gs4_token_path <- paste0(secrets_dir, "/gs4-token.rds")
uemail <- Sys.getenv("UEMAIL")
gargle_key_name <- "GARGLE_KEY"

# # Only need to do this once --------------------------------------------------
# gargle_key <- gargle::secret_make_key()
#
# # Add the key name and key to the .Renviron file
# # Default is to the user scope
# usethis::edit_r_environ()
#
# # Check for and create if not exists dir to hold the project level encrypted
# # secrets
# if (!dir.exists(here(secrets_dir))) {dir.create(here(secrets_dir))}
#
# # Perform interactive browser based authentication
# gs4_auth(email=uemail, cache = FALSE)
#
# # Now save the token, encrypted using key generated earlier
# gargle::secret_write_rds(
#   gs4_token(),
#   here(gs4_token_path),
#   key = gargle_key_name
# )
# #  ---------------------------------------------------------------------------

# Authenticate gs4
gs4_auth(
  token = gargle::secret_read_rds(
    here(gs4_token_path),
    key = gargle_key_name
  ),
  scopes = "spreadsheets.readonly",
  cache = FALSE
)

# Get NFL odds data ------------------------------------------------------------
# Set season and get schedule
season <- "2023" # Need to change for games in 2024!!
schedule <- get_nfl_schedule(season) %>%
  mutate(game_date = ymd_hm(game_date))

# Get the current week based on today's date
reg_season <- 2
time_now <- Sys.time()
current_week <- schedule %>%
  filter(type == reg_season) %>%
  select(week, game_date) %>%
  filter(game_date >= time_now) %>%
  group_by(week) %>%
  summarise(
    week_start = min(game_date),
    week_end = max(game_date)
  ) %>%
  head(n = 1)

current_week_num <- current_week[["week"]]

# Determine which games are in the current week
game_ids <- schedule %>%
  filter(
    type == reg_season,
    week == current_week_num,
    game_date > time_now
  ) %>%
  select(game_id)

# Define function to get odds as espnScrapeR odds broken? ----------------------
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

# Get the odds data for this week's games
get_odds <- function(game_id) {
  tryCatch(
    {
      suppressWarnings(get_nfl_predictor(game_id))
    },
    error = function(cond) {
      message(paste("Error getting odds for `game_id`:", game_id))
      message(conditionMessage(cond))
      # Choose a return value in case of error
      NULL
    }
  )
}
nfl_odds <- NULL
for (game in 1:length(game_ids[["game_id"]])) {
  game_id <- game_ids %>%
    deframe() %>%
    getElement(game)

  game_odds <- get_odds(game_id)
  if (!is.null(game_odds)) {
    nfl_odds <- rbind(nfl_odds, game_odds)
  }
}

# Get the list of available teams to choose from
gsheet_url <- Sys.getenv("SHTADDR")
avail_teams <- read_sheet(gsheet_url, sheet = Sys.getenv("SHTTAB")) %>%
  select(current_week_num + 1) %>%
  slice(77:108) # Row numbers in sheet where the available teams are

# Set up the search string to filter the available teams on
regex_exp <- avail_teams %>%
  drop_na() %>%
  unlist(.) %>%
  paste0("\\b", ., "\\b", collapse = "|") %>%
  regex(ignore_case = TRUE)

weekly_selection <- schedule %>%
  select(
    matchup,
    week,
    game_id,
    game_date,
    home_team_id,
    home_team_full,
    away_team_id,
    away_team_full
  ) %>%
  filter(week == current_week_num) %>%
  mutate(date = with_tz(game_date, "US/Eastern")) %>%
  pivot_longer(
    cols = contains("_team_"),
    names_to = c("location", ".value"),
    names_pattern = "((?:home|away))_(.+)"
  ) %>%
  mutate(regex_result = str_extract_all(team_full, regex_exp)) %>%
  unnest(regex_result) %>%
  select(c(-regex_result, -week, -game_date))

# Keep only available teams
team_selection <- nfl_odds %>%
  right_join(weekly_selection,
    by = join_by(game_id, team_id, team_full)
  ) %>%
  select(
    team_full,
    location,
    matchup,
    date,
    win_proj_fpi
  ) %>%
  drop_na() %>%
  arrange(desc(win_proj_fpi))

# Display a pretty table
gt_tbl <- team_selection %>%
  gt() %>%
  gt_theme_pff() %>%
  gt_color_box(
    columns = win_proj_fpi,
    domain = c(5, 95), width = 50, accuracy = 0.1,
    palette = "pff"
  ) %>%
  tab_header(
    title = str_glue("NFL Game Odds for Week {current_week_num}"),
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

ui <- fluidPage(
  gt_output(outputId = "table")
)

server <- function(input, output, session) {
  output$table <- render_gt(expr = gt_tbl)
}

shinyApp(ui = ui, server = server)
