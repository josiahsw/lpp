#' Positional adjustment
#' 
#' Adjusts the total z-score value so the last player picked at each position
#' is 0. Based on LPP methodology (https://web.archive.org/web/20120725032008/http://www.lastplayerpicked.com/how-the-price-guide-works-part-ii-positional-adjustments/).
#' Ohtani required a specific adjustment. Since he was the only DH and rated so
#' highly, adjusting him down to zero would make no sense. Instead the minimum
#' value and it's absolute value were found. Anyone within that range is 
#' adjusted. Anyone above that range is not adjusted. This easily excludes
#' Ohtani but still allows for situations where some positions may be adjusted
#' down, such as in very shallow leagues. That approach may require more 
#' testing to be generalized to all cases, but certainly works for my primary 
#' use case. This function works for both batters and pitchers.
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
    names(optimal_zscores[1]) == "bat", # bat df must be first in list
    all(names(optimal_zscores) %in% c("bat", "pit"))
    )
  
  pos_adj_projections <- list()
  for (i in 1:length(optimal_zscores)) {
    df <- optimal_zscores[[i]]  # get the current data frame
    
    if (pos_adj == "bat_pit") {
      n_drafted <- sum(df$drafted == TRUE)
      df <- df[order(-df$zSUM), ] # ensures df is sorted properly
      zlpp <- df$zSUM[n_drafted] # zSUM of last player picked
      
      df_adjusted <- df %>% 
        dplyr::mutate(aPOS = zlpp) %>%
        dplyr::mutate(aSUM = zSUM - aPOS) %>%
        dplyr::arrange(desc(aSUM))
      
    } else {
      pos_adj_summary <- df %>%
        dplyr::filter(drafted == TRUE) %>%
        dplyr::group_by(pos) %>%
        dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
      
      positive_positions <- pos_adj_summary %>%
        dplyr::filter(aPOS > 0) %>%
        dplyr::pull(pos) 
      
      if (length(positive_positions) > 0) {
        message_text <- paste("Positions with positive aPOS values:", 
                              paste(positive_positions, collapse = ", "), "|", 
                              pos_adj, 
                              "position adjustment method applied")
        message(message_text)
      }
      # apply position adjustment method and merge with original data
      pos_adj_summary <- pos_adj_summary %>%
        dplyr::mutate(
          aPOS = dplyr::case_when(
            pos_adj == "hold_harmless" & aPOS > 0 ~ max(aPOS[aPOS < 0], na.rm = TRUE),
            pos_adj == "zero_out" & aPOS > 0 ~ 0,
            pos_adj == "DH_to_1B" & pos == "DH" ~ {
              aPOS_1B <- aPOS[pos == "1B"]
              if(length(aPOS_1B) > 0) aPOS_1B else aPOS  # needed so function can still apply to pitching df
            },
            pos_adj == "simple" ~ aPOS,
            TRUE ~ aPOS  # default case
          )
        )
      df_adjusted <- df %>%
        dplyr::left_join(pos_adj_summary, by = "pos") %>%
        dplyr::mutate(aSUM = zSUM - aPOS) %>%
        dplyr::arrange(desc(aSUM))
    }
    if (i == 1) {
      pos_adj_projections$bat <- df_adjusted
    } else if (i == 2) {
      pos_adj_projections$pit <- df_adjusted
    }
  }
  return(pos_adj_projections)
}
