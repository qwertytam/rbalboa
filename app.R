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
#     "glue",
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
# usethis::edit_r_environ(scope = "project")
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
today <- Sys.Date()
current_week <- schedule %>%
  filter(type == reg_season) %>%
  select(week, game_date) %>%
  filter(game_date >= Sys.time()) %>%
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
    game_date > Sys.time()
  ) %>%
  select(game_id)

# Get the odds data for this week's games
nfl_odds <- NULL
for (game in 1:length(game_ids[["game_id"]])) {
  game_id <- game_ids %>%
    deframe() %>%
    getElement(game)
  nfl_odds <- rbind(nfl_odds, get_nfl_odds(game_id))
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

# Calculate the win probability and keep only available teams
team_selection <- nfl_odds %>%
  mutate(prob_pct = if_else(ml < 0,
    ml / (ml - 100),
    100 / (ml + 100)
  )) %>%
  left_join(schedule, by = join_by(game_id)) %>%
  mutate(date = with_tz(game_date, "US/Eastern")) %>%
  select(
    team_full,
    location,
    matchup,
    week,
    date,
    ml,
    prob_pct
  ) %>%
  arrange(ml) %>%
  mutate(result = str_extract_all(team_full, regex_exp)) %>%
  unnest(result) %>%
  select(c(-result, -week))

# Display a pretty table
gt_tbl <- team_selection %>%
  mutate(prob_pct = prob_pct * 100) %>%
  gt() %>%
  tab_spanner(columns = ml:prob_pct, label = "odds") %>%
  gt_theme_pff(
    spanners = c("odds")
  ) %>%
  gt_color_box(
    columns = prob_pct,
    domain = c(5, 95), width = 50, accuracy = 0.1,
    palette = "pff"
  ) %>%
  tab_header(
    title = glue::glue("NFL Game Odds for Week {current_week_num}")
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
    ml = "Money Line",
    prob_pct = "Win Prob %"
  ) %>%
  tab_style(
    style = list(
      cell_borders("bottom", "white"),
      cell_fill(color = "#393c40")
    ),
    locations = cells_column_labels(prob_pct)
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
