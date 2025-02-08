#' Create z-score data for testing the position_adjustment function
#'
#' @inheritParams lpp
#'
#' @returns A list of length 2 containing data frames of batter and pitcher 
#'          z-scores.
#' @noRd
zscore_test_data <- function() {
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  
  clean_projections(batter_projections, pitcher_projections) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
}