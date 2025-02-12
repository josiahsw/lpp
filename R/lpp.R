#' Run Last Player Picked
#'
#' @param bat A data frame of batter projections from fangraphs.
#' @param pit A data frame of pitcher projections from fangraphs.
#' @param keepers A data frame of keepers and values. Defaults to NULL.
#' @param lg A string. "AL", "NL", or "MLB" (the default).
#' @param teams An integer. The number of teams in the league. Defaults to 12.
#' @param budget An integer. The starting auction budget of each team in the 
#'               league. Defaults to 260.
#' @param min_bid An integer. The minimum auction bid. Defaults to 1.
#' @param bat_cat A character vector of league batting categories.
#' @param pit_cat A character vector of league pitching categories.
#' @param bat_pos A named integer vector. The number of batters drafted at each 
#'                position per team.
#' @param pit_pos A named integer vector. The number of pitchers drafted at each 
#'                position per team. 
#' @param bench An integer. The number of bench players drafted by each team.
#' @param pos_adj A string. The position adjustment method. One of "simple", 
#'                "hold_harmless" (the default), "zero_out", "DH_to_1B", 
#'                "bat_pit", or "none".
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher 
#'          auction values.
#' @export
lpp <- function(
    bat = NULL, 
    pit = NULL, 
    keepers = NULL, 
    lg = "MLB", 
    teams = 12, 
    budget = 260, 
    min_bid = 1, 
    bat_cat = c("HR", "R", "RBI", "SB", "OBP"),
    pit_cat = c("WQS", "SVHLD", "SO", "ERA", "WHIP"),
    bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
                "MI" = 1, "OF" = 5, "UT" = 1),
    pit_pos = c("SP" = 6, "RP" = 3, "P" = 0), 
    bench = 2, 
    pos_adj = "hold_harmless"
    ) {
  
  stopifnot(
    !is.null(bat),
    !is.null(pit),
    !is.null(lg), lg %in% c("MLB", "AL", "NL"),
    !is.null(teams), teams > 0, teams %% 1 == 0, # a whole number
    !is.null(budget), budget > 0,
    !is.null(min_bid), min_bid >= 0,
    !is.null(bat_cat),
    all(bat_cat %in% c("HR", "R", "RBI", "SB", "AVG", "OBP")),
    !is.null(pit_cat),
    all(pit_cat %in% c("W", "QS", "WQS", "SV", "HLD", "SVHLD", "SO", "ERA", "WHIP")),
    !is.null(bat_pos), all(bat_pos >= 0),
    setequal(names(bat_pos), c("C", "1B", "2B", "3B", "SS", "CI", "MI", "OF", "UT")),
    !is.null(pit_pos), all(pit_pos >= 0),
    setequal(names(pit_pos), c("SP", "RP", "P")),
    !is.null(bench), bench >= 0,
    !is.null(pos_adj),
    pos_adj %in% c("simple", "hold_harmless", "zero_out", "DH_to_1B", "bat_pit",
                   "none")
  )
  
  clean_projections(bat, pit) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) |>
    position_adjustment(pos_adj)
}