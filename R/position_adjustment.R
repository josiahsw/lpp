#' Positional adjustment
#' 
#' Adjusts the total z-score value based on the position adjustment method
#' selected. The "simple" position adjustment is based on the LPP methodology.
#' https://web.archive.org/web/20120725032008/http://www.lastplayerpicked.com/how-the-price-guide-works-part-ii-positional-adjustments/
#'
#' @param optimal_zscores A list of length 2 containing a data frame of optimal
#'                        batter z-scores and a data frame of optimal pitcher 
#'                        z-scores. The output of find_optimal_zscores().
#' @inheritParams lpp
#'
#' @return A list of length 2 containing the optimal z-scores data frames with 
#'         position adjustment fields added.
#' @noRd
position_adjustment <- function (optimal_zscores, pos_adj) {
  stopifnot(
    length(optimal_zscores) == 2, 
    all(names(optimal_zscores) %in% c("bat", "pit"))
    )
  
  if (pos_adj == "simple") {
    adjusted_zscores <- lapply(optimal_zscores, adj_simple)
  } else if (pos_adj == "none") {
    n_drafted <- lapply(optimal_zscores, find_n_drafted)
    n_drafted <- sum(unlist(n_drafted))
    zlpp <- combined_lpp_zscore(optimal_zscores, n_drafted)
    adjusted_zscores <- lapply(optimal_zscores, add_pos_adj, zlpp = zlpp)
  } else if (pos_adj == "bat_pit"){
    adjusted_zscores <- lapply(optimal_zscores, adj_bat_pit)
  }
  
  return(adjusted_zscores)
}

# helpers -----------------------------------------------------------------
#' Find the number of players marked as drafted
#'
#' @param df A data frame of batter or pitcher projections with drafted players
#'           identified.
#'
#' @returns An integer.
#' @noRd
find_n_drafted <- function(df){
  sum(df$drafted == TRUE)
}

#' Find the z-score of the overall last player picked, ignoring position
#'
#' @inheritParams position_adjustment
#' @param n_drafted An integer, the total number of players to be drafted.
#'
#' @returns Numeric value - the z-score.
#' @noRd
combined_lpp_zscore <- function(optimal_zscores, n_drafted) {
  b <- optimal_zscores[["bat"]]
  p <- optimal_zscores[["pit"]]
  zscores_combined <- sort(c(b$zSUM, p$zSUM), decreasing = TRUE)
  zscores_combined[n_drafted]
}

#' Add position adjustment to data frame
#'
#' @param df A data frame of batter or pitcher z-scores.
#' @param zlpp Numeric, the z-score of the last player picked.
#'
#' @returns The data frame with position adjustment columns added.
#' @noRd
add_pos_adj <- function(df, zlpp) {
  df |>
    dplyr::mutate(aPOS = zlpp) |>
    dplyr::mutate(aSUM = zSUM - aPOS) |>
    dplyr::arrange(dplyr::desc(aSUM))
}

#' Apply bat_pit position adjustment
#'
#' @param df A data frame of batter or pitcher z-scores with drafted players
#'           identified.
#'
#' @returns The data frame with bat_pit adjustment applied.
#' @noRd
adj_bat_pit <- function(df) {
  n_drafted <- find_n_drafted(df)
  df <- df[order(-df$zSUM), ] # ensures df is sorted
  zlpp <- df$zSUM[n_drafted] # last player picked z-score
  add_pos_adj(df, zlpp)
}

#' Apply simple position adjustment and other related position adjustments
#'
#' @param df A data frame of batter or pitcher z-scores with drafted players
#'           identified.
#' @inheritParams lpp
#'
#' @returns The data frame with specified adjustment applied.
#' @noRd
adj_simple <- function(df) {
  summary <- simple_pos_adj_summary(df)
  
  if ("PA" %in% names(df)) {
    aPOS_1B <- summary$aPOS[summary$pos == "1B"]
    summary$aPOS[summary$pos == "DH"] <- aPOS_1B
  }
  
  df |>
    dplyr::left_join(summary, by = "pos") |>
    dplyr::mutate(aSUM = zSUM - aPOS) |>
    dplyr::arrange(dplyr::desc(aSUM))
}

#' Create simple position adjustment summary table
#' 
#' Used in adj_simple() and testing.
#'
#' @param df A data frame of batter or pitcher z-scores with drafted players
#'           identified.
#'
#' @returns A summary data frame of positions and their position adjustments
#' @noRd
simple_pos_adj_summary <- function(df) {
  df |>
    dplyr::filter(drafted == TRUE) |>
    dplyr::group_by(pos) |>
    dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
}

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("aPOS", "aSUM"))