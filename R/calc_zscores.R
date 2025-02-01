#' Calculate z-scores
#' 
#' Calculates z-scores for all categories that are available as
#' initial options then calculates the z-score sum only for selected league-
#' specific categories. The z-scores are then optimized by running the 
#' calculation multiple times until the ranking of draft-able players from the 
#' last iteration is the same as the previous iteration - indicating that a 
#' stable z-score has been achieved or a maximum number of iterations has
#' been reached. This closely follows the method outlined in this article:
#' https://web.archive.org/web/20120725010258/http://www.lastplayerpicked.com/how-the-price-guide-works-part-iv-iterations/ 
#' and also in the comments to Part 1 of the series. I deviate from the article 
#' by iterating at the z-score step without first applying the positional
#' adjustment and calculating player values, since both of those steps
#'
#' @param df 
#' @param categories a vector of batter or pitcher stat categories, from league
#'    settings
#' @param stat "bat" or "pit"
#'
#' @return A data frame with calculated z-scores, sorted by zSUM of selected
#'    league categories
#' @noRd
calc_zscores <- function(df, categories, stat) {
  stat <- match.arg(stat, c("bat", "pit"))
  selected_cols <- paste0("z", categories)
  
  dp_mean <- draftpool_summary(df, mean) # see weight_rate_stats.R for function documentation
  dp_sd <- draftpool_summary(df, stats::sd)
  
  if (stat == "bat") {
    df <- df %>%
      dplyr::mutate(
        zHR = z_score(HR, dp_mean["HR"], dp_sd["HR"]),
        zR = z_score(R, dp_mean["R"], dp_sd["R"]),
        zRBI = z_score(RBI, dp_mean["RBI"], dp_sd["RBI"]),
        zSB = z_score(SB, dp_mean["SB"], dp_sd["SB"]),
        zAVG = z_score(wAVG, dp_mean["wAVG"], dp_sd["wAVG"]),
        zOBP = z_score(wOBP, dp_mean["wOBP"], dp_sd["wOBP"])
      )
  } else {
    df <- df %>%
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
    # sum z-scores for selected categories
    df$zSUM <- rowSums(as.matrix(df[selected_cols]))
    
    return(df)
}

# helpers -----------------------------------------------------------------

#' Calculate z-score
#' 
#' See article for detail:
#' https://web.archive.org/web/20120725032003/http://www.lastplayerpicked.com/how-the-price-guide-works-part-i-standard-scores/
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
