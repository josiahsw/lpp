dollar_values <- function(dfs, bat_pos, pit_pos, bench, teams, budget, min_bid) {

  slots_per_team <- sum(bat_pos) + sum(pit_pos) + bench 
  marginal_dollars <- teams * (budget - (slots_per_team * min_bid))
  value <- calc_total_value(b, p)
  
  results <- lapply(dfs, assign_dollar_value, value = value, 
                    marginal_dollars = marginal_dollars)
  
  return(results)
}

calc_total_value <- function(dfs) {
  b <- dfs$bat
  p <- dfs$pit
  
  bat_value <- b %>%
    dplyr::filter(aSUM > 0) %>%
    dplyr::summarise(value = sum(aSUM)) %>%
    dplyr::pull()
  
  pit_value <- p %>%
    dplyr::filter(aSUM > 0) %>%
    dplyr::summarise(value = sum(aSUM)) %>%
    dplyr::pull()
  
  total_value <- bat_value + pit_value
  
  return(total_value)
}

assign_dollar_value <- function(df, value, marginal_dollars) {
  df %>%
    dplyr::mutate(
      across(
        c(starts_with("z"), aPOS, aSUM), 
        ~ player_value(., value, marginal_dollars), .names = "d{.col}")
    ) %>%
    dplyr::select(-starts_with("z"), -aPOS, -aSUM, -drafted) %>%
    dplyr::rename_with(~ drop_dz_prefix(.), starts_with("dz")) %>%
    dplyr::rename(PTS = SUM, aPOS = daPOS, Dollars = daSUM)
}

player_value <- function(x, value, marginal_dollars) {
  (x / value) * marginal_dollars + 1
}

drop_dz_prefix <- function(name) {
  sub("^dz", "", name)
}