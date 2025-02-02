test_that("weight_rate_stats() assigns drafted values if missing", {
  # check test data
  result <- clean_projections(batter_projections, pitcher_projections)
  expect_true(!all(result$bat$drafted))
  
  n_drafted <- 150
  df <- weight_rate_stats(result$bat, n_drafted, "bat")
  expect_equal(sum(df$drafted), n_drafted)
})

test_that("weight_rate_stats() does not assign drafted values if they exist", {
  # check test data
  result <- clean_projections(batter_projections, pitcher_projections)
  result$bat$drafted[1] <- TRUE
  expect_equal(sum(result$bat$drafted), 1)
  
  n_drafted <- 150
  df <- weight_rate_stats(result$bat, n_drafted, "bat")
  expect_equal(sum(df$drafted), 1)
})

test_that("weight_rate_stats() creates expected columns", {
  result <- clean_projections(batter_projections, pitcher_projections)
  n_drafted <- 150
  wbat <- weight_rate_stats(result$bat, n_drafted, "bat")
  wpit <- weight_rate_stats(result$pit, n_drafted, "pit")
  expect_true(all(c("wAVG", "wOBP") %in% names(wbat)))
  expect_true(all(c("wERA", "wWHIP") %in% names(wpit)))
})

test_that("draftpool_summary() throws error if no drafted players", {
  # check test data
  result <- clean_projections(batter_projections, pitcher_projections)
  expect_true(!all(result$bat$drafted))
  
  expect_error(draftpool_summary(result$bat, mean))
})

test_that("draftpool_summary() returns numeric vector", {
  result <- clean_projections(batter_projections, pitcher_projections)
  result$bat$drafted[1:150] <- TRUE
  
  dp_mean <- draftpool_summary(result$bat, mean)
  expect_true(is.vector(dp_mean))
  expect_true(is.numeric(dp_mean))
})

test_that("draftpool_summary() works for mean and sd", {
  result <- clean_projections(batter_projections, pitcher_projections)
  result$bat$drafted[1:150] <- TRUE
  
  test_mean <- mean(result$bat$HR[result$bat$drafted == TRUE])
  test_sd <- sd(result$bat$HR[result$bat$drafted == TRUE])
  
  dp_mean <- draftpool_summary(result$bat, mean)[["HR"]]
  dp_sd <- draftpool_summary(result$bat, sd)[["HR"]]
  
  expect_equal(dp_mean, test_mean)
  expect_equal(dp_sd, test_sd)
})

test_that("x_above_avg() works", {
  x <- 12
  pt <- 40
  dp_mean <- .25
  
  result <- x_above_avg(x, pt, dp_mean)
  expect_equal(result, 2)
})
