test_that("calc_zscores() returns accurate zSUM", {
  categories <- c("HR", "R", "RBI", "SB", "AVG", "OBP")
  bat_cat <- sample(categories, 5, replace = FALSE) # random selection for test
  z_cols <- paste0("z", bat_cat)
  results <- clean_projections(batter_projections, pitcher_projections)
  n_drafted <- 100
  
  df <- weight_rate_stats(results$bat, n_drafted, "bat") %>%
    calc_zscores(bat_cat, "bat")
  
  df1 <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(zSUM1 = sum(dplyr::across(dplyr::all_of(z_cols)), na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  expect_true("zSUM" %in% colnames(df))
  expect_equal(df$zSUM, df1$zSUM1)
})

test_that("z_score() works", {
  raw_score <- 5
  pop_mean <- 3
  pop_sd <- 2
  result <- z_score(raw_score, pop_mean, pop_sd)
  expect_equal(result, 1)
})