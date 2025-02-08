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

batter_zscore_test_data <- function() {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  
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

pitcher_zscore_test_data <- function() {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  
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