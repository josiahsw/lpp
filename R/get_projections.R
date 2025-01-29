#' Scrape and clean fangraphs batter projections
#' 
#' A wrapper on gardy::fg_batter_projections() that returns only the fields 
#' needed for the auction calculator.
#'
#' @param type projection type
#'
#' @return a tibble
#' @export
get_bat_projections <- function(type) {
  bat_proj <- gardy::fg_batter_projections(type)
  return(bat_proj)
}

#' Scrape and clean fangraphs pitcher projections
#' 
#' A wrapper on gardy::fg_pitcher_projections() that returns only the fields
#' needed for the auction calculator.
#'
#' @param type projection type
#'
#' @return a tibble
#' @export
get_pit_projections <- function(type) {
  pit_proj <- gardy::fg_pitcher_projections(type)
  return(pit_proj)
}

clean_data <- function(df, stat){
  if (stat == "bat") {
    cleaned_data <- df %>%
      mutate(OB = H + BB + HBP, # needed for weighting OBP
             position = str_replace(position, "PH/PR", "OF"),
             most_valuble_position = assign_most_valuble_position(position)
      )
  }
  if (stat == "pit") {
    cleaned_data <- df %>%
      mutate(xQS = ifelse(is.na(QS), calc_QS(GS, ERA, IP), QS),
             position = if_else(GS > 3.5, "SP", "RP")) %>%
      mutate(xQS = replace(xQS, is.nan(xQS), 0)) %>%
      select(-QS)
  }
  return(cleaned_data)
}