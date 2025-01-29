

test_that("top_avail() only accepts valid 'slot' argument", {
  pos <- c("C", "1B", "2B", "SS", "3B", "OF")
  df <- data.frame(
    "zHR" = rnorm(500, mean = 0, sd = .2),
    "zR" = rnorm(500, mean = 0, sd = .35),
    "zRBI" = rnorm(500, mean = 0, sd = .4),
    "zSB" = rnorm(500, mean = 0, sd = .1),
    "zOBP" = rnorm(500, mean = 0, sd = .3),
    "pos" = sample(pos, 500, replace = TRUE),
    "fangraphs_id" = as.character(sample(1:1000, 500, replace = FALSE)),
    "drafted" = FALSE
  )
  
  not_named <- 12
  too_long <- c("C" = 12, "1B" = 12)
  just_right <- c("MI" = 12)
  
  expect_error(top_avail(df, not_named))
  expect_error(top_avail(df, too_long))
  expect_no_error(top_avail(df, just_right))
})

test_that("top_avail() returns NULL if slot == 0", {
  pos <- c("C", "1B", "2B", "SS", "3B", "OF")
  df <- data.frame(
    "zHR" = rnorm(500, mean = 0, sd = .2),
    "zR" = rnorm(500, mean = 0, sd = .35),
    "zRBI" = rnorm(500, mean = 0, sd = .4),
    "zSB" = rnorm(500, mean = 0, sd = .1),
    "zOBP" = rnorm(500, mean = 0, sd = .3),
    "pos" = sample(pos, 500, replace = TRUE),
    "fangraphs_id" = as.character(sample(1:1000, 500, replace = FALSE)),
    "drafted" = FALSE
  )
  
  slot <- c("UT" = 0)
  result <- top_avail(df, slot)
  expect_null(result)
})

test_that("top_avail() returns correct type and length", {
  pos <- c("C", "1B", "2B", "SS", "3B", "OF")
  df <- data.frame(
    "zHR" = rnorm(500, mean = 0, sd = .2),
    "zR" = rnorm(500, mean = 0, sd = .35),
    "zRBI" = rnorm(500, mean = 0, sd = .4),
    "zSB" = rnorm(500, mean = 0, sd = .1),
    "zOBP" = rnorm(500, mean = 0, sd = .3),
    "pos" = sample(pos, 500, replace = TRUE),
    "fangraphs_id" = as.character(sample(1:1000, 500, replace = FALSE)),
    "drafted" = FALSE
  )
  
  slot <- c("3B" = 12)
  result <- top_avail(df, slot)
  expect_vector(result, ptype = character(), size = slot)
})

test_that("mark_drafted_players() works", {
  pos <- c("C", "1B", "2B", "SS", "3B", "OF")
  df <- data.frame(
    "zHR" = rnorm(500, mean = 0, sd = .2),
    "zR" = rnorm(500, mean = 0, sd = .35),
    "zRBI" = rnorm(500, mean = 0, sd = .4),
    "zSB" = rnorm(500, mean = 0, sd = .1),
    "zOBP" = rnorm(500, mean = 0, sd = .3),
    "pos" = sample(pos, 500, replace = TRUE),
    "fangraphs_id" = as.character(sample(1:500, 500, replace = FALSE)),
    "drafted" = FALSE
  )
  # validate test data
  expect_true(sum(df$drafted) == 0)
  
  slot <- c("OF" = 30)
  result <- top_avail(df, slot)
  df <- mark_drafted_players(df, result)
  
  # only correct ids are marked as drafted
  drafted_id <- df$fangraphs_id[df$drafted == TRUE]
  not_drafted <- df$fangraphs_id[df$drafted == FALSE]
  expect_equal(drafted_id, result)
  expect_false(any(result %in% not_drafted))
  
  # only correct pos is drafted
  drafted_pos <- df$pos[df$drafted == TRUE]
  expect_true(all(drafted_pos %in% names(slot)))
  
  # correct number of players drafted
  expect_length(drafted_id, slot)
})

test_that("draft_starters() only accepts valid arguments", {
  pos <- c("C", "1B", "2B", "SS", "3B", "OF", "SP", "RP")
  selected_cols <- c("zHR", "zR", "zRBI", "zSB", "zOBP")
  df <- data.frame(
    "zHR" = rnorm(800, mean = 0, sd = .2),
    "zR" = rnorm(800, mean = 0, sd = .35),
    "zRBI" = rnorm(800, mean = 0, sd = .4),
    "zSB" = rnorm(800, mean = 0, sd = .1),
    "zOBP" = rnorm(800, mean = 0, sd = .3),
    "pos" = sample(pos, 800, replace = TRUE),
    "fangraphs_id" = as.character(sample(1:800, 800, replace = FALSE)),
    "drafted" = FALSE
  )
  df$zSUM <- rowSums(as.matrix(df[selected_cols]))
  
  # no CI
  missing_slot <- c("C" = 12, "1B" = 12, "2B" = 12, "3B" = 12, "SS" = 12, 
                    "OF" = 20, "MI" = 0, "UT" = 0)
  na_slot <- slots <- c("C" = 12, "1B" = 12, "2B" = 12, "3B" = 12, "SS" = 12, 
                        "OF" = 20, "CI" = 0, "MI" = NA, "UT" = 0)
  expect_error(draft_starters(df, missing_slot, "bat"))
  expect_error(draft_starters(df, na_slot, "bat"))
  
  missing_slot <- c("SP" = 12, "RP" = 12)
  na_slot <- slots <- c("SP" = NA, "RP" = 12, "P" = 12)
})


