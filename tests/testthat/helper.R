#' Raw batter and pitcher projections for testing
#' 
#' ATC ROS projections scraped from fangraphs.com in roughly mid-2024.
#' They are good representations of the raw projection data on which the 
#' package will be used. Included here for development and testing so new 
#' projections do not need to be imported each session. 
#'
#' @returns A list of length 2 containing raw batter and pitcher projections
#' @noRd
projection_test_data <- function() {
    bat <- readRDS(testthat::test_path("fixtures", "batter_projections.rds"))
    pit <- readRDS(testthat::test_path("fixtures", "pitcher_projections.rds"))
    list(bat = bat, pit = pit)
}

#' Create test data needed for draft_starters()
#'
#' @returns A data frame of batter z-scores
#' @noRd
batter_zscore_test_data <- function() {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2
  teams = 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  
  n_drafted <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  
  cleaned$bat |>
    weight_rate_stats(n_drafted$bat) |>
    calc_zscores(bat_cat)
}

#' Create test data needed for draft_starters()
#'
#' @returns A data frame of pitcher z-scores
#' @noRd
pitcher_zscore_test_data <- function() {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2
  teams = 12
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  
  n_drafted <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  
  cleaned$pit |>
    weight_rate_stats(n_drafted$pit) |>
    calc_zscores(pit_cat)
}

#' Create list needed for testing draft_starters()
#'
#' @returns List of drafted players by position
#' @noRd
n_drafted_by_pos_test_list <- function() {
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2
  teams = 12
  
  list(
    bat = bat_pos * teams, 
    pit = pit_pos * teams,
    bench = c(bench = bench * teams)
  )
}

#' Create z-score data for testing the position_adjustment function
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
  test_data <- projection_test_data()
  
  clean_projections(test_data$bat, test_data$pit) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
}

#' Create adjusted test data for testing dollar_values()
#'
#' @returns A list of length 2 containing data frames of position adjusted
#'          batter and pitcher data.
#' @noRd
adjusted_test_data <- function() {
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  pos_adj = "simple"
  test_data <- projection_test_data()
  
  clean_projections(test_data$bat, test_data$pit) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) |>
    position_adjustment(pos_adj)
}