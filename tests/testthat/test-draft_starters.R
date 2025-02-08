test_that("number of batters marked as drafted is correct", {
  z <- batter_zscore_test_data()
  n_drafted_by_pos <- n_drafted_by_pos_test_list()
  bat <- z |> draft_starters(n_drafted_by_pos$bat)
  
  expect_equal(sum(bat$drafted == TRUE), sum(n_drafted_by_pos$bat))
})

test_that("number of pitchers marked as drafted is correct", {
  z <- pitcher_zscore_test_data()
  n_drafted_by_pos <- n_drafted_by_pos_test_list()
  pit <- z |> draft_starters(n_drafted_by_pos$pit)
  
  expect_equal(sum(pit$drafted == TRUE), sum(n_drafted_by_pos$pit))
})

test_that("top batters at each position are marked as drafted", {
  z <- batter_zscore_test_data()
  n_drafted_by_pos <- n_drafted_by_pos_test_list()
  bat <- z |> draft_starters(n_drafted_by_pos$bat)
  
  top_pos_drafted <- function(df, pos) {
    df <- df[df$pos == pos, ]
    min_drafted <- min(df$zSUM[df$drafted == TRUE])
    max_undrafted <- max(df$zSUM[df$drafted == FALSE])
    isTRUE(min_drafted > max_undrafted)
  }
  
  pos <- unique(bat$pos)
  result <-  sapply(pos, function(pos) top_pos_drafted(bat, pos))
  
  expect_true(all(result))
})

test_that("top pitchers at each position are marked as drafted", {
  z <- pitcher_zscore_test_data()
  n_drafted_by_pos <- n_drafted_by_pos_test_list()
  pit <- z |> draft_starters(n_drafted_by_pos$pit)
  
  top_pos_drafted <- function(df, pos) {
    df <- df[df$pos == pos, ]
    min_drafted <- min(df$zSUM[df$drafted == TRUE])
    max_undrafted <- max(df$zSUM[df$drafted == FALSE])
    isTRUE(min_drafted > max_undrafted)
  }
  
  pos <- unique(pit$pos)
  result <-  sapply(pos, function(pos) top_pos_drafted(pit, pos))
  
  expect_true(all(result))
})