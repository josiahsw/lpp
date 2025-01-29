test_that("calc_zscores() returns zSUM", {
  df <- data.frame(
    "HR" = sample(0:40, 100, replace = TRUE),
    "R" = sample(20:120, 100, replace = TRUE),
    "RBI" = sample(20:120, 100, replace = TRUE),
    "SB" = sample(0:50, 100, replace = TRUE),
    "wAVG" = rnorm(100, mean = .250, sd = .25),
    "wOBP" = rnorm(100, mean = .320, sd = .3),
    "drafted" = TRUE
  )
  
  # random selection of 5 categories for test
  categories <- c("HR", "R", "RBI", "SB", "AVG", "OBP")
  selected_categories <- sample(categories, 5, replace = FALSE)
  df <- calc_zscores(df, selected_categories, "bat")
  
  expect_true("zSUM" %in% colnames(df))
})
