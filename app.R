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

source(here::here('R', 'get_schedule.R'))
source(here::here('R', 'get_odds.R'))
source(here::here('R', 'make_tables.R'))

# How many weeks to get odds for
num_weeks <- 3

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
schedule <- get_schedule(get_season_dates()) %>%
  mutate(game_date = ymd_hm(game_date))

# Get the next x weeks based on today's date
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
  head(n = num_weeks) %>%
  tail(n = 1)

current_week_num <- current_week[["week"]]

# Determine which games are in the next x weeks
game_ids <- schedule %>%
  filter(
    type == reg_season,
    week <= current_week_num,
    game_date > time_now
  ) %>%
  select(game_id)

game_odds <- NULL
for (game in 1:length(game_ids[["game_id"]])) {
  game_id <- game_ids %>%
    deframe() %>%
    getElement(game)

  game_odd <- get_odds(game_id)
  if (!is.null(game_odd)) {
    game_odds <- rbind(game_odds, game_odd)
  }
}

# Get the list of available teams to choose from
gsheet_url <- Sys.getenv("SHTADDR")
avail_teams <- read_sheet(gsheet_url, sheet = Sys.getenv("SHTTAB")) %>%
  select(min(ncol(.), current_week_num + 1)) %>%
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
  filter(week <= current_week_num) %>%
  mutate(date = with_tz(game_date, "US/Eastern")) %>%
  pivot_longer(
    cols = contains("_team_"),
    names_to = c("location", ".value"),
    names_pattern = "((?:home|away))_(.+)"
  ) %>%
  mutate(regex_result = str_extract_all(team_full, regex_exp)) %>%
  unnest(regex_result) %>%
  select(c(-regex_result, -game_date))

# Keep only available teams
team_selection <- game_odds %>%
  right_join(weekly_selection,
    by = join_by(game_id, team_id, team_full)
  ) %>%
  select(
    team_full,
    location,
    matchup,
    date,
    week,
    win_proj_fpi
  ) %>%
  drop_na() %>%
  arrange(desc(win_proj_fpi))

# Display a pretty table
tbl <- make_table(team_selection, time_now)
tbl

ui <- fluidPage(
  gt_output(outputId = "table")
)

server <- function(input, output, session) {
  output$table <- render_gt(expr = tbl)
}

shinyApp(ui = ui, server = server)
