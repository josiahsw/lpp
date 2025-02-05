test_that("bat_pit pos_adj method works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  test <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj = "bat_pit")
  
  # number of pitchers and batters drafted is the same in control and test
  expect_equal(sum(ctrl$bat$drafted == TRUE), sum(test$bat$aSUM >= 0))
  expect_equal(sum(ctrl$pit$drafted == TRUE), sum(test$pit$aSUM >= 0))
  # top batters and pitchers by z-score are adjusted
  expect_true(min(test$bat$zSUM[test$bat$aSUM >= 0]) > max(test$bat$zSUM[test$bat$aSUM < 0]))
  expect_true(min(test$pit$zSUM[test$pit$aSUM >= 0]) > max(test$pit$zSUM[test$pit$aSUM < 0]))
  # all batters receive the same adj, all pitchers receive the same adj
  expect_true(all(test$bat$aPOS == test$bat$aPOS[1]))
  expect_true(all(test$pit$aPOS == test$pit$aPOS[1]))
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("simple pos_adj method works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  test <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj = "simple")
  
  ctrl_pa_bat <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  ctrl_pa_pit <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  
  # all individual positions receive the same adjustment, and that the adjustment matches the control
  expect_true(all(test$bat$aPOS[test$bat$pos == "1B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "1B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "2B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "2B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "3B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "3B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "C"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "C"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "DH"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "OF"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "OF"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "SS"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "SS"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "SP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "SP"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "RP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "RP"]))
  # all position adjustments are different - this is an assumption
  expect_true(length(unique(ctrl_pa_bat$aPOS)) == length(ctrl_pa_bat$aPOS))
  expect_true(length(unique(ctrl_pa_pit$aPOS)) == length(ctrl_pa_pit$aPOS))
  # positive position adjustments are left alone
  expect_true(sum(test$bat$aPOS > 0) > 0)
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("hold_harmless pos_adj method works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  test <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj = "hold_harmless")
  
  ctrl_pa_bat <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  ctrl_pa_pit <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  
  positive_positions <- ctrl_pa_bat %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  hh_pa <- max(ctrl_pa_bat$aPOS[ctrl_pa_bat$aPOS < 0], na.rm = TRUE)
  
  # positive positions receive hold_harmless adjustment
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == hh_pa))
  # all other individual positions receive the same adjustment, and that adjustment matches the control
  expect_true(all(test$bat$aPOS[test$bat$pos == "1B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "1B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "2B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "2B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "3B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "3B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "C"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "C"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "OF"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "OF"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "SS"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "SS"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "SP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "SP"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "RP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "RP"]))
  # there are no positive position adjustments
  expect_true(sum(test$bat$aPOS > 0) == 0)
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("zero_out pos_adj method works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  test <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj = "zero_out")
  
  ctrl_pa_bat <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  ctrl_pa_pit <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  
  positive_positions <- ctrl_pa_bat %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  
  # positive positions are zeroed out 
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == 0))
  # all other individual positions receive the same adjustment, and that adjustment matches the control
  expect_true(all(test$bat$aPOS[test$bat$pos == "1B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "1B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "2B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "2B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "3B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "3B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "C"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "C"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "OF"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "OF"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "SS"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "SS"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "SP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "SP"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "RP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "RP"]))
  # there are no positive position adjustments
  expect_true(sum(test$bat$aPOS > 0) == 0)
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("DH_to_1B pos_adj method works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  test <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj = "DH_to_1B")
  
  ctrl_pa_bat <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  ctrl_pa_pit <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  
  positive_positions <- ctrl_pa_bat %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  
  # DH position adjustment is replaced with 1B
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "1B"]))
  # all other individual positions receive the same adjustment, and that adjustment matches the control
  expect_true(all(test$bat$aPOS[test$bat$pos == "1B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "1B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "2B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "2B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "3B"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "3B"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "C"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "C"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "OF"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "OF"]))
  expect_true(all(test$bat$aPOS[test$bat$pos == "SS"] == ctrl_pa_bat$aPOS[ctrl_pa_bat$pos == "SS"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "SP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "SP"]))
  expect_true(all(test$pit$aPOS[test$pit$pos == "RP"] == ctrl_pa_pit$aPOS[ctrl_pa_pit$pos == "RP"]))
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("find_n_drafted() works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  
  expect_true("drafted" %in% names(ctrl$bat))
  n_bat <- sum(ctrl$bat$drafted == TRUE)
  expect_equal(find_n_drafted(ctrl$bat), n_bat)
  
  # test in vectorized function
  n_drafted <- lapply(ctrl, find_n_drafted)
  test <- sum(unlist(n_drafted))
  expect_equal(test, n_drafted$bat + n_drafted$pit)
  
  # test in mapply
  zlpp <- mapply(find_nth_zscore, ctrl, n_drafted, SIMPLIFY = FALSE)
  adjusted_zscores <- mapply(add_pos_adj, ctrl, zlpp, SIMPLIFY = FALSE)
  expect_true(all(adjusted_zscores$bat$aPOS == zlpp$bat)) 
  expect_true(all(adjusted_zscores$pit$aPOS == zlpp$pit))
})