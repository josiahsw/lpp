test_that("drafted values are assigned if missing", {
  result <- clean_projections(batter_projections, pitcher_projections)
  expect_true(!all(result$bat$drafted))
  expect_true(!all(result$pit$drafted))
  
  n_drafted <- 150
  bat <- weight_rate_stats(result$bat, n_drafted, "bat")
  pit <- weight_rate_stats(result$pit, n_drafted, "pit")
  expect_equal(sum(bat$drafted), n_drafted)
  expect_equal(sum(pit$drafted), n_drafted)
})

test_that("drafted values are not assigned if they exist", {
  result <- clean_projections(batter_projections, pitcher_projections)
  result$bat$drafted[1] <- TRUE
  result$pit$drafted[1] <- TRUE
  expect_equal(sum(result$bat$drafted), 1)
  expect_equal(sum(result$pit$drafted), 1)
  
  n_drafted <- 150
  bat <- weight_rate_stats(result$bat, n_drafted, "bat")
  pit <- weight_rate_stats(result$pit, n_drafted, "pit")
  expect_equal(sum(bat$drafted), 1)
  expect_equal(sum(pit$drafted), 1)
})

test_that("expected columns are created", {
  result <- clean_projections(batter_projections, pitcher_projections)
  n_drafted <- 150
  wbat <- weight_rate_stats(result$bat, n_drafted, "bat")
  wpit <- weight_rate_stats(result$pit, n_drafted, "pit")
  expect_true(all(c("wAVG", "wOBP") %in% names(wbat)))
  expect_true(all(c("wERA", "wWHIP") %in% names(wpit)))
})