test_that("cleaned batter projections include needed columns", {
  results <- clean_projections(batter_projections, pitcher_projections)
  expect_true(
    all(c("fangraphs_id", "player_name", "OB", "pos", "drafted") %in% 
      colnames(results$bat)))
})

test_that("cleaned pitcher projections include needed columns", {
  results <- clean_projections(batter_projections, pitcher_projections)
  expect_true(
    all(c("fangraphs_id", "player_name", "ER9", "WH", "SVHLD", "pos", "drafted",
          "WQS") %in% colnames(results$pit))) # QS tested in separate test
})

test_that("QS col is created if missing", {
  expect_false("QS" %in% colnames(pitcher_projections)) # validate test data
  results <- clean_projections(batter_projections, pitcher_projections)
  expect_true("QS" %in% colnames(results$pit))
})

test_that("position priority is correct", {
  results <- clean_projections(batter_projections, pitcher_projections)
  minpos <- results$bat$minpos
  pos <- unlist(lapply(minpos, find_priority_pos))
  expect_equal(results$bat$pos, pos)
})

test_that("object returned with expected structure", {
  results <- clean_projections(batter_projections, pitcher_projections)
  expect_true(length(results) == 2)
  expect_true(all(c("bat", "pit") %in% names(results)))
  expect_true(all(sapply(results, is.data.frame)))
})