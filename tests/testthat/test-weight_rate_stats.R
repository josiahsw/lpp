test_that("weight_rate_stats() assigns drafted values if missing", {
  # check test data
  bat <- clean_proj_bat(batter_projections)
  expect_true(!all(bat$drafted))
  
  draft_pool <- 150
  df <- weight_rate_stats(bat, draft_pool, "bat")
  expect_equal(sum(df$drafted), draft_pool)
})

test_that("weight_rate_stats() does not assign drafted values if they exist", {
  # check test data
  bat <- clean_proj_bat(batter_projections)
  bat$drafted[1] <- TRUE
  expect_equal(sum(bat$drafted), 1)
  
  draft_pool <- 150
  df <- weight_rate_stats(bat, draft_pool, "bat")
  expect_equal(sum(df$drafted), 1)
})

test_that("draftpool_summary() throws error if no drafted players", {
  # check test data
  bat <- clean_proj_bat(batter_projections)
  expect_true(!all(bat$drafted))
  
  expect_error(draftpool_summary(bat, mean))
})

test_that("draftpool_summary() returns numeric vector", {
  bat <- clean_proj_bat(batter_projections)
  bat$drafted[1:150] <- TRUE
  
  dp_mean <- draftpool_summary(bat, mean)
  expect_true(is.vector(dp_mean))
  expect_true(is.numeric(dp_mean))
})

test_that("draftpool_summary() works for mean and sd", {
  bat <- clean_proj_bat(batter_projections)
  bat$drafted[1:150] <- TRUE
  
  test_mean <- mean(bat$HR[bat$drafted == TRUE])
  test_sd <- sd(bat$HR[bat$drafted == TRUE])
  
  dp_mean <- draftpool_summary(bat, mean)[["HR"]]
  dp_sd <- draftpool_summary(bat, sd)[["HR"]]
  
  expect_equal(dp_mean, test_mean)
  expect_equal(dp_sd, test_sd)
})
