test_that("cleaned batter projections include needed columns", {
  test_data <- projection_test_data()
  results <- clean_projections(test_data$bat, test_data$pit)
  expect_true(
    all(c("fangraphs_id", "player_name", "OB", "pos", "drafted") %in% 
      colnames(results$bat)))
})

test_that("cleaned pitcher projections include needed columns", {
  test_data <- projection_test_data()
  results <- clean_projections(test_data$bat, test_data$pit)
  expect_true(
    all(c("fangraphs_id", "player_name", "ER9", "WH", "SVHLD", "pos", "drafted",
          "WQS") %in% colnames(results$pit))) # QS tested in separate test
})

test_that("QS col is created if missing", {
  test_data <- projection_test_data()
  expect_false("QS" %in% colnames(test_data$pit)) # validate test data
  results <- clean_projections(test_data$bat, test_data$pit)
  GS <- results$pit$GS
  ERA <- results$pit$ERA
  IP <- results$pit$IP
  QS <- quality_starts(GS, ERA, IP)
  
  expect_true("QS" %in% colnames(results$pit))
  expect_equal(results$pit$QS, QS)
})

test_that("position priority is correct", {
  test_data <- projection_test_data()
  results <- clean_projections(test_data$bat, test_data$pit)
  minpos <- results$bat$minpos
  pos <- unlist(lapply(minpos, find_priority_pos))
  expect_equal(results$bat$pos, pos)
})

test_that("object returned with expected structure", {
  test_data <- projection_test_data()
  results <- clean_projections(test_data$bat, test_data$pit)
  expect_true(length(results) == 2)
  expect_true(all(c("bat", "pit") %in% names(results)))
  expect_true(all(sapply(results, is.data.frame)))
})