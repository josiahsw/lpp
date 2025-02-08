test_that("object returned with expected structure", {
  z <- zscore_test_data()
  pos_adj <- c("none", "bat_pit", "simple", "hold_harmless", "zero_out", 
               "DH_to_1B")
  results <- lapply(pos_adj, position_adjustment, optimal_zscores = z)

  # all elements are length 2
  expect_true(all(sapply(results, function(x) length(x) == 2)))
  
  # all elements contain "bat" and "pit"
  expect_true(
    all(sapply(results, function(x) all(c("bat", "pit") %in% names(x))))
    )
})

test_that("expected columns are created", {
  z <- zscore_test_data()
  pos_adj <- c("none", "bat_pit", "simple", "hold_harmless", "zero_out", 
               "DH_to_1B")
  results <- lapply(pos_adj, position_adjustment, optimal_zscores = z)
  
  expected_columns <- function(df) {
    all(c("aPOS", "aSUM") %in% names(df))
  }
  
  # initial data does not contain expected columns
  expect_false(
    all(sapply(z, function(l) {
      all(sapply(l, expected_columns))
    }))
  )
  
  # results data frames contain expected columns
  expect_true(
    all(sapply(results, function(l) {
      all(sapply(l, expected_columns))
    }))
  )
})

test_that("aSUM is correctly calculated", {
  z <- zscore_test_data()
  pos_adj <- c("none", "bat_pit", "simple", "hold_harmless", "zero_out", 
               "DH_to_1B")
  results <- lapply(pos_adj, position_adjustment, optimal_zscores = z)
  
  aSUM_is_correct <- function(df) {
    all(df$aSUM == df$zSUM - df$aPOS)
  }
  
  # all data frames contain expected columns
  expect_true(
    all(sapply(results, function(l) {
      all(sapply(l, aSUM_is_correct))
    }))
  )
})

test_that("players marked drafted == players with aSUM > 0", {
  z <- zscore_test_data()
  pos_adj <- c("none", "bat_pit", "simple", "hold_harmless", "zero_out", 
               "DH_to_1B")
  results <- lapply(pos_adj, position_adjustment, optimal_zscores = z)
  
  sum_n_drafted <- function(l) {
    lapply(l, find_n_drafted) |>
    unlist() |>
    sum()
  }
  aSUM_over_zero <- function(l) {
    sum(l$bat$aSUM >= 0) + sum(l$pit$aSUM >= 0)
  }
  
  n_drafted <- lapply(results, sum_n_drafted)
  aSUM <- lapply(results, aSUM_over_zero)
  
  expect_equal(n_drafted, aSUM)
})

test_that("pos_adj == none applies same adj to batters and pitchers", {
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "none")
  
  # all batter and pitcher position adjustments are the same
  expect_true(setequal(result$bat$aPOS, result$pit$aPOS))
})

test_that("bat_pit adj is correct", {
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "bat_pit")
  
  # all batters receive same adj, all pitchers receive the same adj
  expect_true(all(result$bat$aPOS == result$bat$aPOS[1]))
  expect_true(all(result$pit$aPOS == result$pit$aPOS[1]))
  
  # batter and pitcher adj are not the same
  expect_false(result$bat$aPOS[1] == result$pit$aPOS[1])
  
  # no players with higher z-scores are below last player picked 
  # (df was properly sorted)
  expect_true(
    min(result$bat$zSUM[result$bat$aSUM >= 0]) > max(result$bat$zSUM[result$bat$aSUM < 0])
    )
  expect_true(
    min(result$pit$zSUM[result$pit$aSUM >= 0]) > max(result$pit$zSUM[result$pit$aSUM < 0])
  )
})

test_that("simple adj is correct", {
  z <- zscore_test_data()
  summary <- lapply(z, simple_pos_adj_summary)
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "simple")
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
  pit_pos <- c("SP", "RP")
  
  # all individual positions receive the adjustment from the control summary
  bat_adj <- sapply(bat_pos, function (pos) {
    all(result$bat$aPOS[result$bat$pos == pos] == 
          summary$bat$aPOS[summary$bat$pos == pos])
  })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(result$pit$aPOS[result$pit$pos == pos] == 
          summary$pit$aPOS[summary$pit$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
  
  # positive positions are left alone
  expect_true(sum(summary$bat$aPOS > 0) > 0)
  expect_true(sum(result$bat$aPOS > 0) > 0)
})

test_that("hold_harmless adj is correct", {
  z <- zscore_test_data()
  summary <- lapply(z, simple_pos_adj_summary)
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "hold_harmless")
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
  pit_pos <- c("SP", "RP")
  
  positive_positions <- summary$bat$pos[summary$bat$aPOS > 0]
  hh_adj <- max(summary$bat$aPOS[summary$bat$aPOS < 0], na.rm = TRUE)
  
  # any positive positions receive the hold harmless adjustment
  expect_true(length(positive_positions) > 0)
  expect_true(all(result$bat$aPOS[result$bat$pos == positive_positions] == 
                    hh_adj))
  
  # all other positions receive the adjustment from the control summary
  bat_pos <- bat_pos[!bat_pos %in% positive_positions]
  bat_adj <- sapply(bat_pos, function (pos) {
    all(result$bat$aPOS[result$bat$pos == pos] == 
          summary$bat$aPOS[summary$bat$pos == pos])
  })
  pit_pos <- pit_pos[!pit_pos %in% positive_positions]
  pit_adj <- sapply(pit_pos, function (pos) {
    all(result$pit$aPOS[result$pit$pos == pos] == 
          summary$pit$aPOS[summary$pit$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
})

test_that("zero_out adj is correct", {
  z <- zscore_test_data()
  summary <- lapply(z, simple_pos_adj_summary)
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "zero_out")
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
  pit_pos <- c("SP", "RP")
  
  positive_positions <- summary$bat$pos[summary$bat$aPOS > 0]
  
  # any positive positions receive the hold harmless adjustment
  expect_true(length(positive_positions) > 0)
  expect_true(all(result$bat$aPOS[result$bat$pos == positive_positions] == 0))
  
  # all other positions receive the adjustment from the control summary
  bat_pos <- bat_pos[!bat_pos %in% positive_positions]
  bat_adj <- sapply(bat_pos, function (pos) {
    all(result$bat$aPOS[result$bat$pos == pos] == 
          summary$bat$aPOS[summary$bat$pos == pos])
  })
  pit_pos <- pit_pos[!pit_pos %in% positive_positions]
  pit_adj <- sapply(pit_pos, function (pos) {
    all(result$pit$aPOS[result$pit$pos == pos] == 
          summary$pit$aPOS[summary$pit$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
})

test_that("DH_to_1B adj is correct", {
  z <- zscore_test_data()
  summary <- lapply(z, simple_pos_adj_summary)
  result <- zscore_test_data() |>
    position_adjustment(pos_adj = "DH_to_1B")
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
  pit_pos <- c("SP", "RP")
  
  # DH adjustment matches 1B adjustment from control summary
  expect_true(all(result$bat$aPOS[result$bat$pos == "DH"] == 
                    summary$bat$aPOS[summary$bat$pos == "1B"]))
  
  # all other positions receive the adjustment from the control summary
  bat_pos <- bat_pos[!bat_pos %in% "DH"]
  bat_adj <- sapply(bat_pos, function (pos) {
    all(result$bat$aPOS[result$bat$pos == pos] == 
          summary$bat$aPOS[summary$bat$pos == pos])
  })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(result$pit$aPOS[result$pit$pos == pos] == 
          summary$pit$aPOS[summary$pit$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
})
