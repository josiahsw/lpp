test_that("i is greater than 2", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  results <- clean_projections(batter_projections, pitcher_projections) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat, max_i = 25, test = TRUE)
  expect_true(length(results) > 2)
})

test_that("iteration doesn't modify nrow, ncol, or colnames", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  results <- clean_projections(batter_projections, pitcher_projections) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat, max_i = 2, test = TRUE)
  i1 <- results[[1]]
  i2 <- results[[2]]
  
  expect_equal(nrow(i1$bat), nrow(i2$bat))
  expect_equal(nrow(i1$pit), nrow(i2$pit))
  expect_equal(ncol(i1$bat), ncol(i2$bat))
  expect_equal(ncol(i1$pit), ncol(i2$pit))
  expect_equal(names(i1$bat), names(i2$bat))
  expect_equal(names(i1$pit), names(i2$pit))
})

test_that("iteration stops when ranks are equal", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  results <- clean_projections(batter_projections, pitcher_projections) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat, max_i = 25, test = TRUE)
  
  i_final <- length(results)
  i_minus_one <- i_final - 1
  i_minus_two <- i_final - 2
  
  i_final_bat_ranks <-     results[[i_final]]$bat$fangraphs_id[results[[i_final]]$bat$drafted == TRUE]
  i_minus_one_bat_ranks <- results[[i_minus_one]]$bat$fangraphs_id[results[[i_minus_one]]$bat$drafted == TRUE]
  i_minus_two_bat_ranks <- results[[i_minus_two]]$bat$fangraphs_id[results[[i_minus_two]]$bat$drafted == TRUE]
  
  i_final_pit_ranks <-     results[[i_final]]$pit$fangraphs_id[results[[i_final]]$pit$drafted == TRUE]
  i_minus_one_pit_ranks <- results[[i_minus_one]]$pit$fangraphs_id[results[[i_minus_one]]$pit$drafted == TRUE]
  i_minus_two_pit_ranks <- results[[i_minus_two]]$pit$fangraphs_id[results[[i_minus_two]]$pit$drafted == TRUE]
  
  
  expect_true(identical(i_final_bat_ranks, i_minus_one_bat_ranks) & 
                identical(i_final_pit_ranks, i_minus_one_pit_ranks))
  expect_false(identical(i_minus_one_bat_ranks, i_minus_two_bat_ranks) & 
                 identical(i_minus_one_pit_ranks, i_minus_two_pit_ranks))
})

test_that("returns expected object", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  results <- clean_projections(batter_projections, pitcher_projections) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  
  expect_true(length(results) == 2)
  expect_true(all(c("bat", "pit") %in% names(results)))
  expect_true(all(sapply(results, is.data.frame)))
})