#' Weight rate stats by playing time
#' 
#' Weights rate stats for batters and pitchers by playing time, so players with 
#' higher playing time are rewarded for having above average rate stats. 
#' Closely follows the method outlined in this article:
#' https://web.archive.org/web/20120725032003/http://www.lastplayerpicked.com/how-the-price-guide-works-part-i-standard-scores/.
#'
#' @param cleaned_df A data frame of cleaned batter or pitcher projections from 
#'                   clean_projections().
#' @param n_drafted An integer, the number of batters or pitchers drafted. 
#'
#' @return The cleaned projection data frame with weighted rate stat variables 
#'         added.
#' @noRd
weight_rate_stats <- function(cleaned_df, n_drafted) {
  # for the first iteration where no players are marked as drafted yet.
  # this will be adjusted for in later iterations.
  if (sum(cleaned_df$drafted) == 0) {
    cleaned_df$drafted[1:n_drafted] <- TRUE
  }
  
  dp_mean <- draftpool_summary(cleaned_df, mean)
  
  if ("PA" %in% names(cleaned_df)) {
    weighted_df <- cleaned_df |>
      dplyr::mutate(
        wAVG = x_above_avg(H, AB, dp_mean["AVG"]), # converts to hits above avg
        wOBP = x_above_avg(OB, PA, dp_mean["OBP"]) # converts to on-base above avg
      )
  } else {
    weighted_df <- cleaned_df |>
      dplyr::mutate(
        wERA = -x_above_avg(ER9, IP, dp_mean["ERA"]), # runs prevented above avg
        wWHIP = -x_above_avg(WH, IP, dp_mean["WHIP"]) # walks + hits prevented above avg
      )
  }
  
  return(weighted_df)
}

# helpers -----------------------------------------------------------------
#' Calculate a summary statistic for a given population of drafted players
#' 
#' A helper function used to calculate the mean or SD of a given player
#' pool. Used in weighting rate stats and calculating z-scores.
#'
#' @param df A data frame of pitcher or batter projections.
#' @param fun A function.
#'
#' @return A numeric vector, the summary stat of each numeric column in the data frame.
#' @noRd
draftpool_summary <- function(df, fun) {
  stopifnot(any(df$drafted))
  
  df |>
    dplyr::filter(drafted == TRUE) |>
    dplyr::summarise_if(is.numeric, fun) |>
    unlist()
}

#' x above average
#' 
#' converts a rate stat to a counting stat based on the LPP method. Can be 
#' interpreted as returning a counting stat above what the average player would 
#' produce given the same amount of playing time.
#'
#' For example, since AVG measures H/AB, wAVG becomes H above the average player 
#' given the same amount of AB. If the average player would get 10 hits in 40 AB 
#' (a .250 AVG), and player y gets 12 hits in 40 AB (a .300 AVG), player y would 
#' have 2 hits above AVG. 
#' 
#' The ERA formula (ER * 9) / IP = ERA is re-written as (ER * 9) = IP * ERA so 
#' it can be weighted using the same x_above_avg() formula as the other stats.
#'
#' @param x Numeric, the stat being prorated in the rate statistic. For example,
#'    H used in calculating AVG.
#' @param pt Numeric, playing time value used in calculating the rate statistic.
#'    For example, at-bats (AB) used in calculating AVG.
#' @param dp_mean Numeric, the draft pool mean of the rate stat. For example, 
#'    the mean batting average (AVG) of the player pool that was drafted based 
#'    on league configurations.
#'
#' @return Numeric, the stat x above/below the mean player.
#' @noRd
x_above_avg <- function(x, pt, dp_mean) {
  x - (pt * dp_mean)
}

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("OB", "ER9", "WH"))