test_that("drafted values are assigned if missing", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  expect_true(!all(cleaned$bat$drafted))
  expect_true(!all(cleaned$pit$drafted))
  
  n_drafted <- 150
  weighted <- lapply(cleaned, weight_rate_stats, n_drafted)
  expect_equal(sum(weighted$bat$drafted), n_drafted)
  expect_equal(sum(weighted$pit$drafted), n_drafted)
})

test_that("drafted values are not assigned if they exist", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
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
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  expect_false(all(c("wAVG", "wOBP") %in% names(cleaned$bat)))
  expect_false(all(c("wERA", "wWHIP") %in% names(cleaned$pit)))
  
  n_drafted <- 150
  weighted <- lapply(cleaned, weight_rate_stats, n_drafted)
  expect_true(all(c("wAVG", "wOBP") %in% names(weighted$bat)))
  expect_true(all(c("wERA", "wWHIP") %in% names(weighted$pit)))
})
