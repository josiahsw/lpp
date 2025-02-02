test_that("allocate_bench_slots() works when bench == 0", {
  bat_pos <- 10
  pit_pos <- 5
  bench <- 0
  teams <- 12
  slots <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  expect_equal(slots$bat, bat_pos * teams)
  expect_equal(slots$pit, pit_pos * teams)
})

test_that("allocate_bench_slots() works when n_bench_drafted is odd", {
  bat_pos <- 10
  pit_pos <- 5
  bench <- 1
  teams <- 15
  n_bench_drafted <- bench * teams
  slots <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  expect_equal(slots$bat, bat_pos * teams + n_bench_drafted %/% 2 + 1)
  expect_equal(slots$pit, pit_pos * teams + n_bench_drafted %/% 2)
})

test_that("allocate_bench_slots() works when n_bench_drafted is even", {
  bat_pos <- 10
  pit_pos <- 5
  bench <- 1
  teams <- 12
  n_bench_drafted <- bench * teams
  slots <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  expect_equal(slots$bat, bat_pos * teams + n_bench_drafted %/% 2)
  expect_equal(slots$pit, pit_pos * teams + n_bench_drafted %/% 2)
})

test_that("iteration works", {
  teams <- 12
  bat_cat = c("HR", "R", "RBI", "SB", "OBP")
  pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP")
  bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
              "MI" = 1, "OF" = 5, "UT" = 1)
  pit_pos = c("SP" = 6, "RP" = 3, "P" = 0)
  bench = 2 
  
  results <- clean_projections(batter_projections, pitcher_projections) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat, max_i = 2, test = TRUE)
  i1 <- results[[1]]
  i2 <- results[[2]]
  
})