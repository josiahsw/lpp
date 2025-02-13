test_that("aSUM_above_0() works", {
  adjusted <- suppressMessages(adjusted_test_data()) 
  
  bat <- adjusted$bat
  bat_aSUM <- sum(bat$aSUM[bat$aSUM > 0])

  pit <- adjusted$pit
  pit_aSUM <- sum(pit$aSUM[pit$aSUM > 0])
  
  total_aSUM <- bat_aSUM + pit_aSUM
  test_aSUM <- aSUM_above_0(adjusted)
  expect_equal(test_aSUM, total_aSUM)
})

test_that("player_value() works", {
  adjusted <- suppressMessages(adjusted_test_data())
  x <- 10
  val <- aSUM_above_0(adjusted)
  tmd <- 2832
  value <- (x / val) * tmd + 1
  
  test_value <- player_value(x, total_value = val, total_marginal_dollars = tmd)
  expect_equal(test_value, value)
})

test_that("assign_dollar_values() creates correct columns", {
  adjusted <- suppressMessages(adjusted_test_data())
  bat <- adjusted$bat
  val <- aSUM_above_0(adjusted)
  tmd <- 2832
  cols <- names(bat)
  zcols <- c(cols[grepl("^z", cols)], "aPOS", "aSUM")
  dzcols <- paste0("d", zcols)
  
  test <- assign_dollar_values(bat, total_value = val, total_marginal_dollars = tmd)
  expect_true(all(dzcols %in% names(test)))
})

test_that("assign_dollar_values() calculates correctly", {
  adjusted <- suppressMessages(adjusted_test_data()) 
  bat <- adjusted$bat
  bat_test <- adjusted$bat
  val <- aSUM_above_0(adjusted)
  tmd <- 2832
  cols <- names(bat)
  zcols <- c(cols[grepl("^z", cols)], "aPOS", "aSUM")
  
  bat[paste0("d", zcols)] <- lapply(bat[zcols], function(x) 
    player_value(x, total_value = val, total_marginal_dollars = tmd))
  
  bat_test <- assign_dollar_values(bat_test, total_value = val, 
                               total_marginal_dollars = tmd)
  expect_equal(bat_test, bat)
})

test_that("cleaned results return correct columns", {
  adjusted <- suppressMessages(adjusted_test_data()) 
  bat <- adjusted$bat
  cols <- names(bat)
  zcols <- c(cols[grepl("^z", cols)])
  dzcols <- paste0("d", zcols)
  final_cols <- c(change_dz_prefix(dzcols), "aPOS", "Dollars")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  test <- dollar_values(adjusted, bat_pos, pit_pos, bench = 2, teams = 12, 
                        budget = 260, min_bid = 1)
  
  expect_true(all(final_cols %in% names(test$bat)))
})
