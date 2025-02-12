test_that("error is thrown if bat is null", {
  test_data <- projection_test_data()
  expect_error(lpp(pit = test_data$pit), class = "simpleError")
})

test_that("error is thrown if pit is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat), class = "simpleError")
})

test_that("error is thrown if lg is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, lg = NULL), 
               class = "simpleError")
})

test_that("error is thrown if lg is not an option", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, lg = "All"), 
    class = "simpleError"
    )
})

test_that("error is thrown if teams is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, teams = NULL), 
               class = "simpleError")
})

test_that("error is thrown if teams is not greater than 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, teams = 0), 
    class = "simpleError"
  )
})

test_that("error is thrown if teams is not a whole number", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, teams = 1.5), 
    class = "simpleError"
  )
})

test_that("error is thrown if budget is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, budget = NULL), 
               class = "simpleError")
})

test_that("error is thrown if budget is not greater than 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, budget = 0), 
    class = "simpleError"
  )
})

test_that("error is thrown if min_bid is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, min_bid = NULL), 
               class = "simpleError")
})

test_that("error is thrown if min_bid is less than 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, min_bid = -1), 
    class = "simpleError"
  )
})

test_that("error is thrown if bat_cat is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, bat_cat = NULL), 
               class = "simpleError")
})

test_that("error is thrown if bat_cat is not an option", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        bat_cat = c("HR", "R", "RBI", "SB", "AVE")), # AVE is incorrect
    class = "simpleError"
  )
})

test_that("error is thrown if pit_cat is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, pit_cat = NULL), 
               class = "simpleError")
})

test_that("error is thrown if pit_cat is not an option", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        pit_cat = c("WQS", "SVHLD", "K", "ERA", "WHIP")), # K is incorrect
    class = "simpleError"
  )
})

test_that("error is thrown if bat_pos is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, bat_pos = NULL), 
               class = "simpleError")
})

test_that("error is thrown if positions missing from bat_pos", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
                    "MI" = 1, "OF" = 5)), # UT missing
    class = "simpleError"
  )
})

test_that("error is thrown if bat_pos elements are not >= 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        bat_pos = c("C" = 1, "1B" = 0, "2B" = 0, "3B" = -1, "SS" = 1, "CI" = 1, 
                    "MI" = 1, "OF" = 5, "UT" = 1)), # 3B is incorrect
    class = "simpleError"
  )
})

test_that("error is thrown if pit_pos is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, pit_pos = NULL), 
               class = "simpleError")
})

test_that("error is thrown if positions missing from pit_pos", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        pit_pos = c("SP" = 5, "RP" = 3)), # P is missing
    class = "simpleError"
  )
})

test_that("error is thrown if pit_pos elements are not >= 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        pit_pos = c("SP" = 5, "RP" = -1, "P" = 0)), # rp incorrect
    class = "simpleError"
  )
})

test_that("error is thrown if bench is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, bench = NULL), 
               class = "simpleError")
})

test_that("error is thrown if bench is not >= 0", {
  test_data <- projection_test_data()
  expect_error(
    lpp(bat = test_data$bat, pit = test_data$pit, 
        bench = -1),
    class = "simpleError"
  )
})

test_that("error is thrown if pos_adj is null", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, pos_adj = NULL), 
               class = "simpleError")
})

test_that("error is thrown if pos_adj is not an option", {
  test_data <- projection_test_data()
  expect_error(lpp(bat = test_data$bat, pit = test_data$pit, 
                   pos_adj = "nope"), 
               class = "simpleError")
})