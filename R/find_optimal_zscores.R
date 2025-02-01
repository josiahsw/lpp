# add documentation
# create test to make sure iterations are > 2
# what's the best name? find optimal z-scores? simulate draft? find optimal draft pool? z-scores

#' Iterate until optimal z-scores/draft pool is found.
#' 
#' Based on Last Player Picked methodology. (add link)
#'
#' @param cleaned_projections A list of length 2 containing the output of clean_projections()
#' @inheritParams lpp
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher z-scores.
#' @noRd 
find_optimal_zscores <- function(cleaned_projections, bat_pos, pit_pos, bench, teams, bat_cat, pit_cat) {
  stopifnot(
    all(names(cleaned_projections) %in% c("bat", "pit")), length(cleaned_projections) == 2
  )
  
  n_drafted <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  
  # variables needed for while loop
  i <- 0
  prior_rank <- NULL
  current_rank <-
    c(head(cleaned_projections$bat$fangraphs_id, n_drafted$bat,
      head(projections$pit$fangraphs_id, n_drafted$pit)))
  max_iterations <- 25
  bat <- cleaned_projections$bat
  pit <- cleaned_projections$pit
  
  while (i < max_iterations && !identical(current_rank, prior_rank)) {
    prior_rank <- current_rank
    
    bat <- bat %>%
      weight_rate_stats(n_drafted, "bat") %>%
      calc_zscores(bat_cat, "bat") %>%
      draft_starters(bat_slots, "bat")
    
    pit <- pit %>%
      weight_rate_stats(n_drafted, "pit") %>%
      calc_zscores(pit_cat, "pit") %>%
      draft_starters(pit_slots, "pit")
    
    # draft bench players
    bench <- top_avail_bench(bat, pit, bench_slots)
    bat <- mark_drafted_players(bat, bench)
    pit <- mark_drafted_players(pit, bench)
    
    current_rank <- combined_rankings(bat, pit)
    i <- i + 1
  }
  
  if (i >= max_iterations) {
    message("Max iterations reached without stable rankings.")
  } else {
    message("Stable rankings reached after ", i, " iterations.")
  }
  
  # clean draft results
  zbat <- paste0("z", bat_cat)
  zpit <- paste0("z", pit_cat)
  
  bat <- bat %>%
    dplyr::select(
      fangraphs_id,
      player_name,
      minpos,
      pos,
      ADP,
      PA,
      dplyr::all_of(zbat),
      zSUM,
      drafted
    ) %>%
    dplyr::arrange(
      desc(zSUM)
    )
  
  pit <- pit %>%
    dplyr::select(
      fangraphs_id,
      player_name,
      pos,
      ADP,
      IP,
      dplyr::all_of(zpit),
      zSUM,
      drafted
    ) %>%
    dplyr::arrange(
      desc(zSUM)
    )
  
  results <- list(
    bat = bat,
    pit = pit
  )
  
  return(results)
}

#' Allocate bench slots between batters and pitchers.
#' 
#' Assumes the number of bench batters and pitchers are equal. If the number is 
#' odd the remainder is distributed to batters. Used for initial iterations 
#' and z-scores.
#'
#' @inheritParams lpp 
#'
#' @returns A list of length 2, the number of batters and pitchers drafted based
#'          on league settings.
#' @noRd
allocate_bench_slots <- function(bat_pos, pit_pos, bench, teams) {
  n_bench_drafted <- bench * teams
  
  if (n_bench_drafted == 0) {
    b <- sum(bat_pos) * teams
    p <- sum(pit_pos) * teams
  } else if (n_bench_drafted %% 2 != 0) {
    b <- sum(bat_pos) * teams + n_bench_drafted %/% 2 + 1
    p <- sum(pit_pos) * teams + n_bench_drafted %/% 2
  } else if (n_bench_drafted %% 2 == 0) {
    b <- sum(bat_pos) * teams + n_bench_drafted / 2
    p <- sum(pit_pos) * teams + n_bench_drafted / 2
  }
  results <- list(bat = b, pit = p)
  return(results)
}