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