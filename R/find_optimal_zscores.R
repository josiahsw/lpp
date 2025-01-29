# add documentation
# create test to make sure iterations are > 2
# add link to lpp method documentation
# what's the best name? find optimal z-scores? simulate draft? find optimal draft pool? z-scores

find_optimal_zscores <- function(projections, draft_pool, categories, slots) {
  stopifnot(
    all(names(projections) %in% c("bat", "pit")), length(projections) == 2,
    all(names(draft_pool) %in% c("bat", "pit")), length(draft_pool) == 2,
    all(names(categories) %in% c("bat", "pit")), length(categories) == 2,
    all(names(slots) %in% c("bat", "pit", "bench")), length(slots) == 3
  )
  
  # variables needed for while loop
  i <- 0
  prior_rank <- NULL
  current_rank <-
    c(head(projections$bat$fangraphs_id, draft_pool$bat),
      head(projections$pit$fangraphs_id, draft_pool$pit))
  max_iterations <- 25
  bat <- projections$bat
  pit <- projections$pit
  
  while (i < max_iterations && !identical(current_rank, prior_rank)) {
    prior_rank <- current_rank
    
    bat <- bat %>%
      weight_rate_stats(draft_pool$bat, "bat") %>%
      calc_zscores(lg$categories$bat, "bat") %>%
      draft_starters(slots$bat, "bat")
    
    pit <- pit %>%
      weight_rate_stats(draft_pool$pit, "pit") %>%
      calc_zscores(lg$categories$pit, "pit") %>%
      draft_starters(slots$pit, "pit")
    
    # draft bench players
    bench <- top_avail_bench(bat, pit, slots$bench)
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
  zbat <- paste0("z", lg$categories$bat)
  zpit <- paste0("z", lg$categories$pit)
  
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