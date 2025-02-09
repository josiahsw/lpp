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

test_that("find_priority_pos() returns single positions", {
  expect_equal(find_priority_pos("C"), "C")
  expect_equal(find_priority_pos("SS"), "SS")
  expect_equal(find_priority_pos("2B"), "2B")
  expect_equal(find_priority_pos("3B"), "3B")
  expect_equal(find_priority_pos("DH"), "DH")
})

test_that("find_priority_pos() returns the priority position", {
  expect_equal(find_priority_pos("C/OF"), "C")
  expect_equal(find_priority_pos("3B/SS/DH"), "SS")
  expect_equal(find_priority_pos("2B/3B"), "2B")
  expect_equal(find_priority_pos("3B/OF"), "3B")
  expect_equal(find_priority_pos("1B/OF"), "OF")
  expect_equal(find_priority_pos("DH/1B"), "1B")
})

test_that("find_priority_pos() can work with vectorized operations", {
  minpos <- c("C/OF", "3B/SS/DH", "2B/3B", "3B/OF", "1B/OF", "DH/1B")
  expected <- c("C", "SS", "2B", "3B", "OF", "1B")
  result <- purrr::map_chr(minpos, find_priority_pos)
  expect_equal(result, expected)
})
