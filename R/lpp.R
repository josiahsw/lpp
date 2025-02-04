#' Run Last Player Picked
#'
#' @param bat A data frame of batter projections.
#' @param pit A data frame of pitcher projections.
#' @param keepers A data frame of keepers and values. Defaults to NULL.
#' @param lg League: "AL", "NL", or "MLB" (the default).
#' @param teams An integer. The number of teams in the league. Defaults to 12.
#' @param budget An integer. The starting auction budget of each team in the league. Defaults to 260.
#' @param min_bid An integer. The minimum auction bid. Defaults to 1.
#' @param bat_cat A character vector of league batting categories.
#' @param pit_cat A character vector of league pitching categories.
#' @param bat_pos A named integer vector. The number of batters drafted at each position per team.
#' @param pit_pos A named integer vector. The number of pitchers drafted at each position per team.
#' @param bench An integer. The number of bench players drafted by each team.
#' @param pos_adj Position adjustment method. 
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher auction values.
#' @export
#' @examples
#' lpp(batter_projections, pitcher_projections)
#' 
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
    pos_adj = c("hold_harmless", "zero_out", "DH_to_1B", "simple", "bat_pit", "none")
    ) {
  
  clean_projections(bat, pit) %>%
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) %>%
    position_adjustment(pos_adj)
}