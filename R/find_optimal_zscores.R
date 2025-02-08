#' Iterate until optimal z-scores are found.
#' 
#' Based on Last Player Picked methodology. (add link)
#'
#' @param cleaned_projections A list of length 2 containing the output of 
#'                            clean_projections()
#' @inheritParams lpp
#' @param max_i An integer, the maximum number of iterations. Used for testing.
#'              Defaults to 25.
#' @param test Select TRUE if testing the function, which will save the results 
#'             of each iteration for inspection. Defaults to FALSE.
#'
#' @returns A list of length 2 containing a data frame of batter and pitcher 
#'          z-scores.
#' @importFrom utils head
#' @noRd 
find_optimal_zscores <- function(cleaned_projections, bat_pos, pit_pos, bench, 
                                 teams, bat_cat, pit_cat, 
                                 max_i = 25, test = FALSE) {
  stopifnot(
    all(names(cleaned_projections) %in% c("bat", "pit")), 
    length(cleaned_projections) == 2
    )
  
  bat <- cleaned_projections$bat
  pit <- cleaned_projections$pit
  n_drafted <- allocate_bench_slots(bat_pos, pit_pos, bench, teams)
  n_drafted_by_pos <- list(
    bat = bat_pos * teams, 
    pit = pit_pos * teams,
    bench = c(bench = bench * teams)
    )
  # variables needed for while loop
  i <- 0
  prior_rank <- NULL
  current_rank <-
    c(utils::head(cleaned_projections$bat$fangraphs_id, n_drafted$bat,
      utils::head(cleaned_projections$pit$fangraphs_id, n_drafted$pit)))
  max_iterations <- max_i
  test_list <- list()  # only used for testing
  
  while (i < max_iterations && !identical(current_rank, prior_rank)) {
    prior_rank <- current_rank
    
    bat <- bat |>
      weight_rate_stats(n_drafted$bat) |>
      calc_zscores(bat_cat) |>
      draft_starters(n_drafted_by_pos$bat)
    
    pit <- pit |>
      weight_rate_stats(n_drafted$pit) |>
      calc_zscores(pit_cat) |>
      draft_starters(n_drafted_by_pos$pit)
    
    # draft bench players
    bench <- top_avail_bench(bat, pit, n_drafted_by_pos$bench)
    bat <- mark_drafted_players(bat, bench)
    pit <- mark_drafted_players(pit, bench)
    
    current_rank <- combined_rankings(bat, pit)
    i <- i + 1
    
    if (test == TRUE) {
      test_list[[i]] <- list(bat = bat, pit = pit)
    }
  }
  
  if (test == TRUE) {
    return(test_list)
  } else {
    
    if (i >= max_iterations) {
      message("Max iterations reached without stable rankings.")
    } else {
      message("Stable rankings reached after ", i, " iterations.")
    }
    
    # clean draft results
    zbat <- paste0("z", bat_cat)
    zpit <- paste0("z", pit_cat)
    
    bat <- bat |>
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
      ) |>
      dplyr::arrange(
        dplyr::desc(zSUM)
      )
    
    pit <- pit |>
      dplyr::select(
        fangraphs_id,
        player_name,
        pos,
        ADP,
        IP,
        dplyr::all_of(zpit),
        zSUM,
        drafted
      ) |>
      dplyr::arrange(
        dplyr::desc(zSUM)
      )
    
    results <- list(bat = bat, pit = pit)
    return(results)
  }
}

# helpers -----------------------------------------------------------------
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

# dplyr unquoted variable names to eliminate notes when running R CMD check
utils::globalVariables(c("fangraphs_id", "player_name", "pos", "zSUM", 
                         "drafted"))