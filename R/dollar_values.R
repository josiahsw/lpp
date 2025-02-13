#' Calculate dollar values
#' 
#' Closely follows the method from this article:
#' https://web.archive.org/web/20120725022740/http://www.lastplayerpicked.com/how-the-price-guide-works-part-iii-dollar-values/.
#'
#' @param adjusted_dfs A list of length 2 containing data frames of position
#'                     adjusted batter and pitcher data. The output of 
#'                     position_adjustment().
#' @inheritParams lpp 
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher
#'          dollar values.
#' @noRd
dollar_values <- function(adjusted_dfs, bat_pos, pit_pos, bench, teams, budget, 
                          min_bid) {
  slots_per_team <- sum(bat_pos) + sum(pit_pos) + bench 
  val <- aSUM_above_0(adjusted_dfs)
  tmd <- teams * (budget - (slots_per_team * min_bid))
  
  results <- lapply(adjusted_dfs, assign_dollar_values, 
         total_value = val, total_marginal_dollars = tmd)
  
  lapply(results, clean_results)
}

# helpers ----------------------------------------------------------------------
#' Find total adjusted aSUM above 0 for all drafted players (batters and 
#' pitchers)
#'
#' @inheritParams  dollar_values
#'
#' @returns Numeric, total aSUM above zero
#' @noRd
aSUM_above_0 <- function(adjusted_dfs) {
  bat <- adjusted_dfs$bat
  bat_aSUM <- sum(bat$aSUM[bat$aSUM > 0])
  
  pit <- adjusted_dfs$pit
  pit_aSUM <- sum(pit$aSUM[pit$aSUM > 0])
  
  bat_aSUM + pit_aSUM
}

#' Add dollar values to a data frame
#'
#' @param adjusted_df A data frame of position adjusted batter or pitcher data.
#' @param total_value Numeric, the total aSUM (adjusted z_score sum) for all 
#'                    drafted players. 
#' @param total_marginal_dollars Numeric, the total amount of league dollars 
#'                               available to spend.
#'
#' @returns The adjusted data frame dollar values columns added.
#' @noRd
assign_dollar_values <- function(adjusted_df, total_value, total_marginal_dollars) {
  adjusted_df |>
    dplyr::mutate(
      dplyr::across(
        c(dplyr::starts_with("z"), aPOS, aSUM), 
        ~ player_value(., total_value, total_marginal_dollars), .names = "d{.col}")
    )
}

#' Convert z-score to dollar value
#' 
#' Finds the proportion of a players z-score to the total z-score value, then
#' takes the same proportion of available dollars to find the dollar value.
#'
#' @param x Numeric, the z-score.
#' @param total_value The total z-score value of drafted players.
#' @param total_marginal_dollars The total amount of dollars available to be
#'                               spent on players.
#'
#' @returns Numeric, the converted dollar value
#' @noRd
player_value <- function(x, total_value, total_marginal_dollars) {
  (x / total_value) * total_marginal_dollars + 1
}

#' Clean results data frame
#'
#' @param df The dollar value assigned data frame.
#'
#' @returns The data frame with unneeded columns removed and others renamed.
#' @noRd
clean_results <- function(df) {
  df |>
    dplyr::select(-dplyr::starts_with("z"), -aPOS, -aSUM, -drafted) |>
    dplyr::rename_with(~ change_dz_prefix(.), dplyr::starts_with("dz")) |>
    dplyr::rename(aPOS = daPOS, Dollars = daSUM) |>
    dplyr::arrange(dplyr::desc(Dollars))
}

#' Change colname prefix
#'
#' @param name colname
#'
#' @returns colname with changed prefix
#' @noRd
change_dz_prefix <- function(name) {
  sub("^dz", "d", name)
}

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("daPOS", "daSUM", "Dollars"))