test_that("cleaned projections include needed columns", {
  bat <- clean_proj_bat(batter_projections)
  pit <- clean_proj_pit(pitcher_projections)
  expect_true(all(
    c("fangraphs_id", "player_name", "OB", "pos", "drafted") %in% colnames(bat))
  )
  expect_true(all(
    c("fangraphs_id", "player_name", "ER9", "WH", "SVHLD", "WQS", "pos", 
      "drafted") %in% colnames(pit))
  )
})

test_that("clean_proj_pit() creates a QS col if missing", {
  expect_false("QS" %in% colnames(pitcher_projections))
  df <- clean_proj_pit(pitcher_projections)
  expect_true("QS" %in% colnames(df))
})

test_that("multi_pos_adj() returns single positions", {
  expect_equal(multi_pos_adj("C"), "C")
  expect_equal(multi_pos_adj("SS"), "SS")
  expect_equal(multi_pos_adj("2B"), "2B")
  expect_equal(multi_pos_adj("3B"), "3B")
  expect_equal(multi_pos_adj("DH"), "DH")
})

test_that("multi_pos_adj() returns the priority position", {
  expect_equal(multi_pos_adj("C/OF"), "C")
  expect_equal(multi_pos_adj("3B/SS/DH"), "SS")
  expect_equal(multi_pos_adj("2B/3B"), "2B")
  expect_equal(multi_pos_adj("3B/OF"), "3B")
  expect_equal(multi_pos_adj("1B/OF"), "OF")
  expect_equal(multi_pos_adj("DH/1B"), "1B")
})

test_that("multi_pos_adj() can work with vectorized operations", {
  pos <- c("C/OF", "3B/SS/DH", "2B/3B", "3B/OF", "1B/OF", "DH/1B")
  expect_equal(multi_pos_adj(pos), c("C", "SS", "2B", "3B", "OF", "1B"))
})

test_that("quality_starts() returns 0 and no error if GS == 0", {
  GS <- 0
  ERA <- .345
  IP <- 60
  expect_equal(quality_starts(GS, ERA, IP), 0)
  expect_no_error(quality_starts(GS, ERA, IP))
})

test_that("quality_starts() can work with vectorized operations", {
  df <- data.frame(
    GS = c(10, 20, 30), 
    ERA = c(.300, .350, .325), 
    IP = c(100, 125, 150)
    )
  
  expect_equal(
    quality_starts(df$GS, df$ERA, df$IP), 
    c(11.8561507, 18.0242508, 24.3013985)
    )
})


