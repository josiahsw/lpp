test_that("object returned with expected structure", {
  z <- suppressMessages(zscore_test_data())
  pos_adj <- c("none", "bat_pit", "simple")
  results <- lapply(pos_adj, position_adjustment, optimal_zscores = z)

  # all elements are length 2
  expect_true(all(sapply(results, function(x) length(x) == 2)))
  
  # all elements contain "bat" and "pit"
  expect_true(
    all(sapply(results, function(x) all(c("bat", "pit") %in% names(x))))
    )
})

test_that("expected columns are created", {
  z <- suppressMessages(zscore_test_data())
  pos_adj <- c("none", "bat_pit", "simple")
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
  z <- suppressMessages(zscore_test_data())
  pos_adj <- c("none", "bat_pit", "simple")
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
  z <- suppressMessages(zscore_test_data())
  pos_adj <- c("none", "bat_pit", "simple")
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
  result <- suppressMessages(zscore_test_data()) |>
    position_adjustment(pos_adj = "none")
  
  # all batter and pitcher position adjustments are the same
  expect_true(setequal(result$bat$aPOS, result$pit$aPOS))
})

test_that("bat_pit adj is correct", {
  result <- suppressMessages(zscore_test_data()) |>
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

test_that("simple adj is correct for all positions other than DH", {
  z <- suppressMessages(zscore_test_data())
  summary <- lapply(z, simple_pos_adj_summary)
  result <- suppressMessages(zscore_test_data()) |>
    position_adjustment(pos_adj = "simple")
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF")
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
})

test_that("simple adj replaces DH aPOS with 1B", {
  z <- suppressMessages(zscore_test_data())
  summary <- simple_pos_adj_summary(z$bat)
  
  if ("PA" %in% names(z$bat)) {
    aPOS_1B <- summary$aPOS[summary$pos == "1B"]
    summary$aPOS[summary$pos == "DH"] <- aPOS_1B
  }
  
  test <- adj_simple(z$bat)
  expect_true(all(test$aPOS[test$pos == "DH"] == aPOS_1B))
})
