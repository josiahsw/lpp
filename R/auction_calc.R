# Auction calculator
# 2024-04-21


# load packages -----------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(dbplyr)
library(purrr)
library(gardy)

# global variables --------------------------------------------------------
CATEGORIES <- list(
  BAT = c("HR", "R", "RBI", "SB", "AVG", "OBP"),
  PIT = c("W", "QS", "WQS", "SV", "HLD", "SVHLD", "SO", "ERA", "WHIP"))
lg <- c("MLB", "AL", "NL")
teams <- 12

# inputs / selections -----------------------------------------------------
league <- "MLB"
teams <- 12
budget <- 260
min_bid <- 1
bat_cat <- c("HR", "R", "RBI", "SB", "OBP")
pit_cat <- c("W+QS", "SV+HLD", "SO", "ERA", "WHIP")
bat_pos <- c(
  "C"  = 1,
  "1B" = 1,
  "2B" = 1, 
  "3B" = 1, 
  "SS" = 1, 
  "CI" = 1, 
  "MI" = 1, 
  "OF" = 5, 
  "UT" = 1
  )
pit_pos = c(
  "SP" = 6,
  "RP" = 3, 
  "P"  = 0
  )
bench <- 4

## old settings
lg <- list(
  player_pool  = "MLB",  # MLB, AL, or NL
  teams = 12, # enter number of teams in league, 10, 12, or 15
  auction = list(
    budget = 260,
    min_bid = 1 
  ),
  categories = list(
    bat = CATEGORIES$BAT[c( # select batting categories from CATEGORIES$BAT
      "HR"  = TRUE, 
      "R"   = TRUE, 
      "RBI" = TRUE, 
      "SB"  = TRUE, 
      "AVG" = FALSE, 
      "OBP" = TRUE
    )],
    pit = CATEGORIES$PIT[c( # select pitching categories from CATEGORIES$PIT
      "W"      = FALSE, 
      "QS"     = FALSE, 
      "WQS"    = TRUE, 
      "SV"     = FALSE, 
      "HLD"    = FALSE, 
      "SVHLD"  = TRUE, 
      "SO"     = TRUE, 
      "ERA"    = TRUE, 
      "WHIP"   = TRUE
    )]
  ),
  positions = list(
    bat = c( # enter number of players at each position per team
      "C"  = 1,
      "1B" = 1,
      "2B" = 1,
      "3B" = 1,
      "SS" = 1,
      "CI" = 1,
      "MI" = 1,
      "OF" = 5,
      "UT" = 1
    ),
    pit = c( # enter number of players at each position per team
      "SP" = 6,
      "RP" = 3,
      "P"  = 0
    ),
    bench =  c("bench" = 4)  # enter bench slots per team  
  )
)

## calculator settings
calc <- list(
  type = "steamer_ros",   # select from proj_type
  bat_split = .7,
  rp_decr = NULL,
  keepers = FALSE # include list of keepers in projections
)

# calculated inputs -------------------------------------------------------
## a vector of the total number of players drafted at each slot
slots <- list(
  bat = lg$positions$bat * lg$teams,
  pit = lg$positions$pit * lg$teams,
  bench = lg$positions$bench * lg$teams
)

## number of batters/pitchers drafted, initially allocates half of bench players
## to bat/pit (this is adjusted during iterations)
draft_pool <- list(
  bat = sum(slots$bat) + (slots$bench[[1]] / 2),
  pit = sum(slots$pit) + (slots$bench[[1]] / 2)
)

zbat <- paste0("z", lg$categories$bat)
zpit <- paste0("z", lg$categories$pit)

# functions ---------------------------------------------------------------
source("./R/clean_projections.R")
source("./R/weight_rate_stats.R")
source("./R/calc_zscores.R")
source("./R/draft_starters.R")
source("./R/find_optimal_zscores.R")
source("./R/position_adjustment.R")

# import test data -------------------------------------------------------------
load("./data/batter_projections.rda")
load("./data/pitcher_projections.rda")

bat <- batter_projections %>%
  clean_proj_bat()

pit <- pitcher_projections %>%
  clean_proj_pit()

projections <- list(
  bat = bat,
  pit = pit
)

# tests -------------------------------------------------------------------
wbat <- projections$bat %>% weight_rate_stats(draft_pool$bat, "bat")
zbat <- wbat %>% calc_zscores(lg$categories$bat, "bat")
sbat <- zbat %>% draft_starters(slots$bat, "bat")

wpit <- projections$pit %>% weight_rate_stats(draft_pool$pit, "pit")
zpit <- wpit %>% calc_zscores(lg$categories$pit, "pit")
spit <- zpit %>% draft_starters(slots$pit, "pit")

# combine bat and pit data frames to find top avail bench players
bench <- top_avail_bench(sbat, spit, slots$bench)
bbat <- mark_drafted_players(sbat, bench)
bpit <- mark_drafted_players(spit, bench)


i <- 0
prior_rank <- NULL
current_rank <-
  c(head(projections$bat$fangraphs_id, draft_pool$bat),
    head(projections$pit$fangraphs_id, draft_pool$pit))
max_iterations <- 25
bat <- projections$bat
pit <- projections$pit

while (i < max_iterations && !identical(current_rank, prior_rank)) {
  prior_rank <- current_rank
  
  bat <- bat %>%
    weight_rate_stats(draft_pool$bat, "bat") %>%
    calc_zscores(lg$categories$bat, "bat") %>%
    draft_starters(slots$bat, "bat")
  
  pit <- pit %>%
    weight_rate_stats(draft_pool$pit, "pit") %>%
    calc_zscores(lg$categories$pit, "pit") %>%
    draft_starters(slots$pit, "pit")
  
  # draft bench players
  bench <- top_avail_bench(bat, pit, slots$bench)
  bat <- mark_drafted_players(bat, bench)
  pit <- mark_drafted_players(pit, bench)
  
  current_rank <- combined_rankings(bat, pit)
  i <- i + 1
  message("Iteration: ", i)
  print(bat, n = 50)
  print(pit, n = 50)
}

if (i >= max_iterations) {
  message("Max iterations reached without stable draft rankings.")
} else {
  message("Stable draft rankings reached after ", i, " iterations.")
}


# code -------------------------------------------------------------

z_results <- find_optimal_zscores(projections, draft_pool, lg$categories, slots)

p_results <- position_adjustment(z_results)

test <- test$bat

slots_per_team * (budget - (slots_per_team * min_bid))


z_results$pit


