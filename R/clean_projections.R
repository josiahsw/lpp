#' Clean batter and pitcher projections
#' 
#' Data preparation for further calculation occurs in this step, such as
#' creating columns, selecting columns, dealing with weird values etc.
#'
#' @inheritParams lpp
#'
#' @returns A list of length 2 containing a data frame of cleaned batter and 
#'          pitcher projections.
#' @noRd 
clean_projections <- function(bat, pit) {
  bat <- bat %>%
    dplyr::select(
      fangraphs_id = playerid,
      player_name = PlayerName,
      minpos,
      team = Team,
      ADP,
      G,
      AB,
      PA,
      H,
      HR,
      R,
      RBI,
      BB,
      HBP,
      SB,
      AVG,
      OBP,
      SLG
    ) %>%
    dplyr::filter(minpos != "PH/PR") %>%
    dplyr::filter(minpos != "P") %>%
    dplyr::mutate(
      OB = H + BB + HBP, # needed for weighting OBP
      pos = purrr::map_chr(minpos, find_priority_pos),
      drafted = FALSE # needed for calculating z-score
    )
  
  if (!"QS" %in% names(pit)) {
    pit$QS <- as.double(NA)
  }
  
  pit <- pit %>%
    dplyr::select(
      fangraphs_id = playerid,
      player_name = PlayerName,
      team = Team,
      ADP,
      GS,
      G,
      IP,
      W,
      QS,
      SV,
      HLD,
      H,
      ER,
      SO,
      BB,
      ERA,
      WHIP
    ) %>%
    dplyr::mutate(
      xQS = ifelse(is.na(QS), quality_starts(GS, ERA, IP), QS),
      ER9 = ER * 9, # needed for weighting ERA
      WH = BB + H, # needed for weighting WHIP
      SVHLD = SV + HLD,
      pos = dplyr::if_else(GS > 3.5, "SP", "RP"),
      drafted = FALSE # needed for z-score
    ) %>%
    dplyr::select(-QS) %>%
    dplyr::rename(QS = xQS) %>%
    dplyr::mutate(
      WQS = W + QS
    ) 
  
  results <- list(bat = bat, pit = pit)
  return(results)
}

# helpers -----------------------------------------------------------------
#' Find the priority position if multiple positions are listed. 
#'
#' Position priority reference table is hard-coded into the function.
#' The ability to change the position priority reference table could be added as 
#' future enhancement if needed. 
#'
#' @param minpos A string. The minpos field from fangraphs.com batter 
#'          projections which can include several positions separated by a '/'.
#'
#' @return A string. The priority position for each player.
#' @noRd
find_priority_pos <- function(minpos) {
  position_priority = c(
    "C"  = 1, 
    "SS" = 2, 
    "2B" = 3, 
    "3B" = 4, 
    "OF" = 5, 
    "1B" = 6, 
    "DH" = 7
  )
  split_pos <- unlist(strsplit(minpos, "/"))
  names(which.min(position_priority[split_pos]))
}

#' Calculate expected quality starts
#' 
#' A Cup of Fantasy Joe xQS formula.
#'
#' @param GS Numeric, projected games started
#' @param ERA Numeric, projected earned run average
#' @param IP Numeric, projected innings pitched
#'
#' @return Numeric, the expected number of quality starts for an SP
#' @noRd
quality_starts <- function(GS, ERA, IP) {
  # update QS formula, see old files (((IP/GS) / 6.15) - (.11 * ((ER/IP)*9)) * GS) / IP
  ifelse(GS != 0, GS * (.4650115 - (ERA * .0872381) + (IP/GS * .0746775)), 0)
}

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("playerid", "PlayerName", "minpos", "Team", "ADP",
                         "G", "AB", "PA", "H", "HR", "R", "RBI", "BB", "HBP",
                         "SB", "AVG", "OBP", "SLG", "GS", "IP", "W", "QS", "SV",
                         "HLD", "ER", "SO", "ERA", "WHIP", "xQS"))
