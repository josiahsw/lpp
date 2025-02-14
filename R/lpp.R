#' Run Last Player Picked
#'
#' @param bat A data frame of batter projections from fangraphs.
#' @param pit A data frame of pitcher projections from fangraphs.
#' @param keepers A data frame of keepers and values. Defaults to NULL.
#' @param lg A string. The league type. One of "MLB" (the default), "AL", or 
#'           "NL".
#' @param teams A whole number greater than 0. The number of teams in the 
#'              league. Defaults to 12.
#' @param budget A numeric value greater than 0. The starting auction budget of 
#'               each team in the league. Defaults to 260.
#' @param min_bid A numeric value greater than or equal to 0. The minimum 
#'                bid per player. Defaults to 1.
#' @param bat_cat A character vector of league batting stat categories.
#'                Available options are HR, R, RBI, SB, AVG, and OBP. The
#'                default is c("HR", "R", "RBI", "SB", "AVG").
#' @param pit_cat A character vector of league pitching stat categories. 
#'                Available options are W, QS, WQS, SV, HLD, SVHLD, SO, ERA,
#'                and WHIP. If quality starts are not included in the pitcher
#'                projections expected QS will be automatically calculated based
#'                on the Cup of Fantasy Joe xQS formula. The default is c("W", 
#'                "SV", "SO", "ERA", "WHIP").
#' @param bat_pos A named numeric vector where batting position is the name and 
#'                the number of batters drafted at that position per team is the 
#'                value. The default is c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, 
#'                "SS" = 1, "CI" = 1, "MI" = 1, "OF" = 5, "UT" = 1). All 
#'                position names in the default vector must be given and all 
#'                values must be greater than or equal to 0. 
#' @param pit_pos A named numeric vector where pitching position is the name and 
#'                the number of pitchers drafted at each position per team is 
#'                the value. The default is c("SP" = 5, "RP" = 3, "P" = 0). All 
#'                position names in the default vector must be given and all 
#'                values must be greater than or equal to 0.
#' @param bench A numeric value, the number of bench players drafted by each 
#'              team. Defaults to 2. 
#' @param pos_adj A string, the position adjustment method.
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher 
#'          auction values.
#' @export
lpp <- function(
    bat = NULL, 
    pit = NULL, 
    keepers = NULL, 
    lg = c("MLB", "AL", "NL"),
    teams = 12, 
    budget = 260, 
    min_bid = 1, 
    bat_cat = c("HR", "R", "RBI", "SB", "AVG"),
    pit_cat = c("W", "SV", "SO", "ERA", "WHIP"),
    bat_pos = c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1, "SS" = 1, "CI" = 1, 
                "MI" = 1, "OF" = 5, "UT" = 1),
    pit_pos = c("SP" = 5, "RP" = 3, "P" = 0), 
    bench = 2, 
    pos_adj = c("hold_harmless", "zero_out", "DH_to_1B", "simple", "bat_pit",
                "none")
    ) {
  
  lg <- match.arg(lg)
  pos_adj <- match.arg(pos_adj)
  
  stopifnot(
    !is.null(bat),
    !is.null(pit),
    !is.null(teams), teams > 0, 
    "'teams' must be a whole number" = teams %% 1 == 0,
    !is.null(budget), budget > 0,
    !is.null(min_bid), min_bid >= 0,
    !is.null(bat_cat),
    "all elements of 'bat_cat' must be one of the available options. See ?lpp for more detail." = 
      all(bat_cat %in% c("HR", "R", "RBI", "SB", "AVG", "OBP")),
    !is.null(pit_cat),
    "all elements of 'pit_cat' must be one of the available options. See ?lpp for more detail." = 
      all(pit_cat %in% c("W", "QS", "WQS", "SV", "HLD", "SVHLD", "SO", "ERA", "WHIP")),
    !is.null(bat_pos), all(bat_pos >= 0),
    "'bat_pos' must include all default batter positions. See ?lpp for more detail." = 
      setequal(names(bat_pos), c("C", "1B", "2B", "3B", "SS", "CI", "MI", "OF", "UT")),
    !is.null(pit_pos), all(pit_pos >= 0),
    "'pit_pos' must include all default pitcher positions. See ?lpp for more detail." = 
     setequal(names(pit_pos), c("SP", "RP", "P")),
    !is.null(bench), bench >= 0
  )
  
  clean_projections(bat, pit) |>
    find_optimal_zscores(bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) |>
    position_adjustment(pos_adj) |>
    dollar_values(bat_pos, pit_pos, bench, teams, budget, min_bid)
}