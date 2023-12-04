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