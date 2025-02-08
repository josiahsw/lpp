test_that("batter zSUM is accurate", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  categories <- c("HR", "R", "RBI", "SB", "AVG", "OBP")
  bat_cat <- sample(categories, 5, replace = FALSE) # random selection for test
  z_cols <- paste0("z", bat_cat)
  n_drafted <- 100
  
  z <- weight_rate_stats(cleaned$bat, n_drafted) |>
    calc_zscores(bat_cat)
  
  test <- z |>
    dplyr::rowwise() |>
    dplyr::mutate(zSUM1 = sum(dplyr::across(dplyr::all_of(z_cols)), na.rm = TRUE)) |>
    dplyr::ungroup()
  
  expect_equal(z$zSUM, test$zSUM1)
})

test_that("pitcher zSUM is accurate", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  categories <- c("WQS", "SVHLD", "SO", "ERA", "WHIP", "W")
  pit_cat <- sample(categories, 5, replace = FALSE) # random selection for test
  z_cols <- paste0("z", pit_cat)
  n_drafted <- 100
  
  z <- weight_rate_stats(cleaned$pit, n_drafted) |>
    calc_zscores(pit_cat)
  
  test <- z |>
    dplyr::rowwise() |>
    dplyr::mutate(zSUM1 = sum(dplyr::across(dplyr::all_of(z_cols)), na.rm = TRUE)) |>
    dplyr::ungroup()
  
  expect_equal(z$zSUM, test$zSUM1)
})

test_that("expected batter columns are created", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  n_drafted <- 100
  bat_cat <- c("HR", "R", "RBI", "SB", "OBP")
  z_cols <- c("zHR", "zR", "zRBI", "zSB", "zAVG", "zOBP", "zSUM")
  z <- weight_rate_stats(cleaned$bat, n_drafted) |>
    calc_zscores(bat_cat)
  
  expect_true(all(z_cols %in% names(z)))
})

test_that("expected pitcher columns are created", {
  cleaned <- clean_projections(batter_projections, pitcher_projections)
  n_drafted <- 100
  pit_cat <- c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  z_cols <- c("zW", "zQS", "zWQS", "zSV", "zHLD", "zSVHLD", "zSO", "zERA", 
              "zWHIP", "zSUM")
  z <- weight_rate_stats(cleaned$pit, n_drafted) |>
    calc_zscores(pit_cat)
  
  expect_true(all(z_cols %in% names(z)))
})