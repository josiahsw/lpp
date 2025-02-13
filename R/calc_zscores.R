#' Calculate z-scores
#' 
#' Calculates z-scores for all categories that are available as
#' initial options then calculates the z-score sum for selected league-
#' specific categories. Closely follows the method outlined in this article:
#' https://web.archive.org/web/20120725032003/http://www.lastplayerpicked.com/how-the-price-guide-works-part-i-standard-scores/. 
#'
#' @param weighted_df  A data frame of weighted batter or pitcher projections 
#'                     from weight_rate_stats().
#' @param categories A vector of batter or pitcher categories. Either 
#'                   "bat_cat" or "pit_cat".
#'
#' @return The weighted data frame with z-score variables added.
#' @noRd
calc_zscores <- function(weighted_df, categories) {
  selected_cols <- paste0("z", categories)
  dp_mean <- draftpool_summary(weighted_df, mean)
  dp_sd <- draftpool_summary(weighted_df, stats::sd)
  
  if ("PA" %in% names(weighted_df)) {
    zscore_df <- weighted_df |>
      dplyr::mutate(
        zHR = z_score(HR, dp_mean["HR"], dp_sd["HR"]),
        zR = z_score(R, dp_mean["R"], dp_sd["R"]),
        zRBI = z_score(RBI, dp_mean["RBI"], dp_sd["RBI"]),
        zSB = z_score(SB, dp_mean["SB"], dp_sd["SB"]),
        zAVG = z_score(wAVG, dp_mean["wAVG"], dp_sd["wAVG"]),
        zOBP = z_score(wOBP, dp_mean["wOBP"], dp_sd["wOBP"])
      )
  } else {
    zscore_df <- weighted_df |>
      dplyr::mutate(
        zW = z_score(W, dp_mean["W"], dp_sd["W"]),
        zQS = z_score(QS, dp_mean["QS"], dp_sd["QS"]),
        zWQS = z_score(WQS, dp_mean["WQS"], dp_sd["WQS"]),
        zSV = z_score(SV, dp_mean["SV"], dp_sd["SV"]),
        zHLD = z_score(HLD, dp_mean["HLD"], dp_sd["HLD"]),
        zSVHLD = z_score(SVHLD, dp_mean["SVHLD"], dp_sd["SVHLD"]),
        zSO = z_score(SO, dp_mean["SO"], dp_sd["SO"]),
        zERA = z_score(wERA, dp_mean["wERA"], dp_sd["wERA"]),
        zWHIP = z_score(wWHIP, dp_mean["wWHIP"], dp_sd["wWHIP"])
      )
  }
  
  # sum z-scores for selected categories -- base R 100x faster than dplyr
  zscore_df$zSUM <- rowSums(as.matrix(zscore_df[selected_cols]))
  return(zscore_df)
}

# helpers -----------------------------------------------------------------
#' Calculate z-score
#'
#' @param raw_score Numeric
#' @param pop_mean Numeric
#' @param pop_sd Numeric
#'
#' @return Numeric, z-score
#' @noRd
z_score <- function(raw_score, pop_mean, pop_sd) {
  (raw_score - pop_mean) / pop_sd
}

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("wAVG", "wOBP", "WQS", "SVHLD", "wERA", "wWHIP"))
