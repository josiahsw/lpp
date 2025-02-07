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
  
  ctrl_bat <- sum(ctrl$bat$drafted == TRUE)
  ctrl_pit <- sum(ctrl$pit$drafted == TRUE)
  test <- lapply(ctrl, find_n_drafted)
  
  expect_equal(ctrl_bat, test$bat)
  expect_equal(ctrl_pit, test$pit)
})

test_that("combined_lpp_zscore() works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  
  n_drafted <- lapply(ctrl, find_n_drafted)
  n_drafted <- sum(unlist(n_drafted))
  ctrl_zSUM <- sort(c(ctrl$bat$zSUM, ctrl$pit$zSUM), decreasing = TRUE)
  ctrl_zlpp <- ctrl_zSUM[n_drafted]
  test_zlpp <- combined_lpp_zscore(ctrl, n_drafted)
  
  # find the correct z-score
  expect_equal(ctrl_zlpp, test_zlpp)
})

test_that("add_pos_adj() works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  
  expect_false(all(c("aPOS", "aSUM") %in% names(ctrl$bat)))
  
  zlpp <- -2
  test <- add_pos_adj(ctrl$bat, zlpp)
  
  # correct columns are created
  expect_true(all(c("aPOS", "aSUM") %in% names(test)))
  
  # aPOS values are correct
  expect_true(all(test$aPOS == zlpp))
  
  # aSUM calculates correctly
  expect_equal(test$aSUM, test$zSUM - test$aPOS)
})

test_that("pos_adj == none works", {
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
    position_adjustment(pos_adj = "none")
  
  n_drafted <- lapply(ctrl, find_n_drafted)
  n_drafted <- sum(unlist(n_drafted))
  zlpp <- combined_lpp_zscore(ctrl, n_drafted)
  ctrl <- lapply(ctrl, add_pos_adj, zlpp = zlpp)
  
  # output is expected
  expect_true(length(test) == 2)
  expect_true(all(names(test) %in% c("bat", "pit")))
  expect_true(all(sapply(test, is.data.frame)))
  
  # all batter and pitcher position adjustments are the same
  expect_true(setequal(test$bat$aPOS, test$pit$aPOS))
  
  # the number of players drafted == the number of players where aSUM >= 0
  b <- sum(test$bat$aSUM >= 0)
  p <- sum(test$pit$aSUM >= 0)
  expect_equal(n_drafted, b + p)
  
  # control and test are the same
  expect_identical(ctrl, test)
})

test_that("adj_bat_pit() works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  bat <- ctrl$bat
  
  n_drafted <- find_n_drafted(bat)
  bat <- bat[order(-bat$zSUM), ] # ensures df is sorted
  zlpp <- bat$zSUM[n_drafted] # last player picked z-score
  ctrl <- add_pos_adj(bat, zlpp)
  test <- adj_bat_pit(bat)
  
  # control and test are the same
  expect_identical(ctrl, test)
  
  # control and test position adjustment are the same
  expect_true(all(ctrl$aPOS == test$aPOS))
  
  # number of batters drafted == number of players where position adjustment > 0
  expect_equal(sum(ctrl$drafted == TRUE), sum(test$aSUM >= 0))
  
  # no players with higher z-scores are below last player picked (df was properly sorted )
  expect_true(min(test$zSUM[test$aSUM >= 0]) > max(test$zSUM[test$aSUM < 0]))
  
  # all batters receive the same adj
  expect_true(all(test$aPOS == test$aPOS[1]))

  # aSUM calculates correctly
  expect_equal(ctrl$aSUM, test$zSUM - test$aPOS)
})

test_that("pos_adj == bat_pit works", {
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
  ctrl <- lapply(ctrl, adj_bat_pit)
  
  # output is expected
  expect_true(length(test) == 2)
  expect_true(all(names(test) %in% c("bat", "pit")))
  expect_true(all(sapply(test, is.data.frame)))
  
  # control and test are the same
  expect_identical(ctrl, test)
  
  # all batters receive the same adj, all pitchers receive the same adj
  expect_true(all(test$bat$aPOS == test$bat$aPOS[1]))
  expect_true(all(test$pit$aPOS == test$pit$aPOS[1]))
  
  # batter and pitcher adjustments are not the same
  expect_false(test$bat$aPOS[1] == test$pit$aPOS[1])
})

test_that("adj_simple() works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  ctrl <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat)
  bat_summary <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  pit_summary <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  btest <- adj_simple(ctrl$bat, "simple")
  ptest <- adj_simple(ctrl$pit, "simple")
  
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")
  pit_pos <- c("SP", "RP")
  
  # correct columns are created
  expect_true(all(c("aPOS", "aSUM") %in% names(btest)))
  
  # all individual positions receive the adjustment from the control summary
  bat_adj <- sapply(bat_pos, function (pos) {
    all(btest$aPOS[btest$pos == pos] == bat_summary$aPOS[bat_summary$pos == pos])
        })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(ptest$aPOS[ptest$pos == pos] == pit_summary$aPOS[pit_summary$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
  
  # positive position adjustments are left alone
  expect_true(sum(bat_summary$aPOS > 0) > 0)
  expect_true(sum(btest$aPOS > 0) > 0)
  
  # aSUM calculates correctly
  expect_equal(btest$aSUM, btest$zSUM - btest$aPOS)
})

test_that("pos_adj == simple method works", {
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
  ctrl <- lapply(ctrl, adj_simple, pos_adj = "simple")
  
  # output is expected
  expect_true(length(test) == 2)
  expect_true(all(names(test) %in% c("bat", "pit")))
  expect_true(all(sapply(test, is.data.frame)))
  
  # control and test are the same
  expect_identical(ctrl, test)
})

test_that("pos_adj == hold_harmless method works", {
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
  
  bat_summary <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  pit_summary <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  positive_positions <- bat_summary %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  hh_pa <- max(bat_summary$aPOS[bat_summary$aPOS < 0], na.rm = TRUE)
  
  # positive positions receive control hold_harmless adjustment
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == hh_pa))

  # all other individual positions receive adjustment from the control summary
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF") # DH removed
  pit_pos <- c("SP", "RP")
  bat_adj <- sapply(bat_pos, function (pos) {
    all(test$bat$aPOS[test$bat$pos == pos] == bat_summary$aPOS[bat_summary$pos == pos])
  })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(test$aPOS[test$pos == pos] == pit_summary$aPOS[pit_summary$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
  
  # there are no positive position adjustments
  expect_true(sum(test$bat$aPOS > 0) == 0)

  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})

test_that("pos_adj == zero_out method works", {
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
  
  bat_summary <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  pit_summary <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  positive_positions <- bat_summary %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  
  # positive positions are zeroed out 
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == 0))
  
  # all other individual positions receive adjustment from the control summary
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF") # DH removed
  pit_pos <- c("SP", "RP")
  bat_adj <- sapply(bat_pos, function (pos) {
    all(test$bat$aPOS[test$bat$pos == pos] == bat_summary$aPOS[bat_summary$pos == pos])
  })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(test$aPOS[test$pos == pos] == pit_summary$aPOS[pit_summary$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))

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
  
  bat_summary <- ctrl$bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  pit_summary <- ctrl$pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::group_by(pos) %>%
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
  positive_positions <- bat_summary %>%
    dplyr::filter(aPOS > 0) %>%
    dplyr::pull(pos) 
  
  # DH position adjustment is replaced with 1B
  expect_equal(positive_positions, "DH")
  expect_true(all(test$bat$aPOS[test$bat$pos == "DH"] == bat_summary$aPOS[bat_summary$pos == "1B"]))
  
  # all other individual positions receive adjustment from the control summary
  bat_pos <- c("C", "1B", "2B", "3B", "SS", "OF") # DH removed
  pit_pos <- c("SP", "RP")
  bat_adj <- sapply(bat_pos, function (pos) {
    all(test$bat$aPOS[test$bat$pos == pos] == bat_summary$aPOS[bat_summary$pos == pos])
  })
  pit_adj <- sapply(pit_pos, function (pos) {
    all(test$aPOS[test$pos == pos] == pit_summary$aPOS[pit_summary$pos == pos])
  })
  expect_true(all(bat_adj))
  expect_true(all(pit_adj))
  
  # there are no positive position adjustments
  expect_true(sum(test$bat$aPOS > 0) == 0)
  
  # aSUM calculates correctly
  expect_equal(test$bat$aSUM, test$bat$zSUM - test$bat$aPOS)
})