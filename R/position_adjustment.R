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
#' @return A list of length 2 containing position adjusted batter z-scores and
#'         position adjusted pitcher z-scores.
#' @noRd
position_adjustment <- function (optimal_zscores, pos_adj) {
  pos_adj_method <- match.arg(pos_adj_method)
  # need to make sure bat df is first in input list, or else results frame will be named wrong
  # Initialize an empty list to store results
  dfs_adjusted <- list()
  
  # Loop over each data frame in the input list and apply position adjustment
  for (i in 1:length(optimal_zscores)) {
    df <- optimal_zscores[[i]]  # Get the current data frame
    
    if (pos_adj_method == "none") {
      # Step 1: Find count of players drafted
      last_player_picked <- df %>%
        dplyr::filter(drafted == TRUE) %>%
        dplyr::count() %>%
        dplyr::pull(n)
      
      # Step 2: Find zSUM of last player picked, disregarding position
      lpp_zscore <- df %>% 
        dplyr::arrange(-zSUM) %>% 
        pull(zSUM) %>% 
        .[last_player_picked]
      
      # Step 3: Add result to original data frame and create aSUM column
      df_adjusted <- df %>% 
        dplyr::mutate(aPOS = lpp_zscore) %>%
        dplyr::mutate(aSUM = zSUM - aPOS) %>%
        dplyr::arrange(desc(aSUM))
      
    } else {
      # Step 1: Create position adjustment summary
      pos_adj <- df %>%
        dplyr::filter(drafted == TRUE) %>%
        dplyr::group_by(pos) %>%
        dplyr::summarise(aPOS = min(zSUM, na.rm = TRUE))
      
      # Identify positions with positive aPOS values
      positive_positions <- pos_adj %>%
        dplyr::filter(aPOS > 0) %>%
        dplyr::pull(pos) 
      
      # Create message if there are positive aPOS values
      if (length(positive_positions) > 0) {
        message_text <- paste("Positions with positive aPOS values:", 
                              paste(positive_positions, collapse = ", "), "-", 
                              pos_adj_method, 
                              "position adjustment method applied")
        message(message_text)
      }
      
      # Step 2: Apply position adjustment method
      pos_adj <- pos_adj %>%
        dplyr::mutate(
          aPOS = case_when(
            pos_adj_method == "hold_harmless" & aPOS > 0 ~ max(aPOS[aPOS < 0], na.rm = TRUE),
            pos_adj_method == "zero_out" & aPOS > 0 ~ 0,
            pos_adj_method == "DH_to_1B" & pos == "DH" ~ {
              # Get the aPOS value for 1B and apply it to DH
              aPOS_1B <- aPOS[pos == "1B"]
              if(length(aPOS_1B) > 0) aPOS_1B else aPOS
            },
            pos_adj_method == "simple" ~ aPOS,  # Initial position adjustments used for "simple"
            TRUE ~ aPOS  # Default case
          )
        )
      
      # Step 3: Merge with original data frame and create aSUM column
      df_adjusted <- df %>%
        dplyr::left_join(pos_adj, by = "pos") %>%
        dplyr::mutate(aSUM = zSUM - aPOS) %>%
        dplyr::arrange(desc(aSUM))
    }
    
    # Assign names to the list based on the data frame type
    if (i == 1) {
      dfs_adjusted$bat <- df_adjusted  # For batters
    } else if (i == 2) {
      dfs_adjusted$pit <- df_adjusted  # For pitchers
    }
  }
  return(dfs_adjusted)
}
