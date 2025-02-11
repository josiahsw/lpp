test_that("drafted values are assigned if missing", {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  expect_true(!all(cleaned$bat$drafted))
  expect_true(!all(cleaned$pit$drafted))
  
  n_drafted <- 150
  weighted <- lapply(cleaned, weight_rate_stats, n_drafted)
  expect_equal(sum(weighted$bat$drafted), n_drafted)
  expect_equal(sum(weighted$pit$drafted), n_drafted)
})

test_that("drafted values are not assigned if they exist", {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  cleaned$bat$drafted[1] <- TRUE
  cleaned$pit$drafted[1] <- TRUE
  expect_equal(sum(cleaned$bat$drafted), 1)
  expect_equal(sum(cleaned$pit$drafted), 1)
  
  n_drafted <- 150
  weighted <- lapply(cleaned, weight_rate_stats, n_drafted)
  expect_equal(sum(weighted$bat$drafted), 1)
  expect_equal(sum(weighted$pit$drafted), 1)
})

test_that("expected columns are created", {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  expect_false(all(c("wAVG", "wOBP") %in% names(cleaned$bat)))
  expect_false(all(c("wERA", "wWHIP") %in% names(cleaned$pit)))
  
  n_drafted <- 150
  weighted <- lapply(cleaned, weight_rate_stats, n_drafted)
  expect_true(all(c("wAVG", "wOBP") %in% names(weighted$bat)))
  expect_true(all(c("wERA", "wWHIP") %in% names(weighted$pit)))
})

test_that("batter stats are calculated correctly", {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  bat <- cleaned$bat
  n_drafted <- 150
  bat$drafted[1:n_drafted] <- TRUE
  dp_mean <- draftpool_summary(bat, mean)
  wAVG <- x_above_avg(bat$H, bat$AB, dp_mean["AVG"])
  wOBP <- x_above_avg(bat$OB, bat$PA, dp_mean["OBP"])

  weighted <- weight_rate_stats(bat, n_drafted)
  expect_equal(weighted$wAVG, wAVG)
  expect_equal(weighted$wOBP, wOBP)
})

test_that("pitcher stats are calculated correctly", {
  test_data <- projection_test_data()
  cleaned <- clean_projections(test_data$bat, test_data$pit)
  pit <- cleaned$pit
  n_drafted <- 150
  pit$drafted[1:n_drafted] <- TRUE
  dp_mean <- draftpool_summary(pit, mean)
  wERA <- -x_above_avg(pit$ER9, pit$IP, dp_mean["ERA"])
  wWHIP <- -x_above_avg(pit$WH, pit$IP, dp_mean["WHIP"])
  
  weighted <- weight_rate_stats(pit, n_drafted)
  expect_equal(weighted$wERA, wERA)
  expect_equal(weighted$wWHIP, wWHIP)
})
