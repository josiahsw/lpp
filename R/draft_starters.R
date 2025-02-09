#' Draft starting (non-bench) batters or pitchers
#' 
#' Simulates a draft. For example, identifies top players available at each slot 
#' by z-score and marks them as drafted. Starting with main slots (1B, 2B, 
#' etc.), then marks the next available MI and CI, then fills the remaining top 
#' players available at UT.
#'
#' @param zscores A data frame of z-scored batter or pitcher projections. The 
#'                output from calc_zscores().
#' @param n_drafted_by_pos A named vector, the number of batters or pitchers 
#'                         drafted at each position. 
#' 
#' @return The zscores data frame with updated drafted column.
#' @noRd
draft_starters <- function(zscores, n_drafted_by_pos) {
  zscores$drafted <- FALSE # needs to be reset before each iteration
  zscores <- zscores[order(-zscores$zSUM), ] # ensure df is sorted properly
  
  if ("PA" %in% names(zscores)) {
    stopifnot(
      all(c("C", "1B", "2B", "3B", "SS", "OF", "MI", "CI", "UT") %in% 
            names(n_drafted_by_pos)))
    
    main <- n_drafted_by_pos[c("C", "1B", "2B", "3B", "SS", "OF")]
    ids <- sapply(names(main), function(slot) find_top_avail(zscores, main[slot]))
    ids <- unlist(ids)
    results <- mark_drafted_players(zscores, ids)
    
    multi <- n_drafted_by_pos[c("CI", "MI")]
    ids <- sapply(names(multi), function(slot) find_top_avail(results, multi[slot]))
    ids <- unlist(ids)
    results <- mark_drafted_players(results, ids)
    
    ids <- find_top_avail(results, n_drafted_by_pos["UT"])
    results <- mark_drafted_players(results, ids)
  } else {
    stopifnot(all(c("SP", "RP", "P") %in% names(n_drafted_by_pos)))
    
    main <- n_drafted_by_pos[c("SP", "RP")]
    ids <- sapply(names(main), function(slot) find_top_avail(zscores, main[slot]))
    ids <- unlist(ids)
    results <- mark_drafted_players(zscores, ids)
    
    ids <- find_top_avail(results, n_drafted_by_pos["P"])
    results <- mark_drafted_players(results, ids)
  }
  
  return(results)
}

# helpers -----------------------------------------------------------------
#' Identify top available players
#' 
#' A helper that returns a vector of the top available players at a slot by 
#' z-score. For example, for the slot "CI = 12" it will return a vector
#' of player ids of the top 12 un-drafted players eligible for the CI slot. 
#'
#' @param df A data frame of player projections with currently drafted/undrafted
#'          players identified.
#' @param slot A named vector, where the name is the slot and the value is the 
#'    number of players drafted at that slot.
#'
#' @return A character vector of fangraphs_ids of the top available players at a 
#'    slot.
#' @noRd
find_top_avail <- function(df, slot) {
  if (slot == 0) {
    return()
  }
  
  if (names(slot) == "CI") {
    df <- subset(df, !drafted & (pos == "1B" | pos == "3B"))
  } else if (names(slot) == "MI") {
    df <- subset(df, !drafted & (pos == "SS" | pos == "2B"))
  } else if (names(slot) %in% c("UT", "P", "bench")) {
    df <- subset(df, !drafted)
  } else {
    df <- subset(df, !drafted & pos == names(slot))
  }
  
  top_avail <- head(df$fangraphs_id, n = slot)
  return(top_avail)
}

#' Mark drafted players
#'
#' @inheritParams find_top_avail
#' @param ids A character vector of fangraphs_ids of the top available 
#'            players at a slot. The output of find_top_avail().
#'
#' @return The data frame with top available players marked as drafted = TRUE.
#' @noRd
mark_drafted_players <- function(df, ids) {
  df$drafted <- ifelse(df$fangraphs_id %in% top_avail, T, df$drafted)
  return(df)
}

#' Identify top available bench players
#' 
#' Combines batter and pitcher data frames to find top remaining un-drafted 
#' players.
#'
#' @param df_bat A data frame of drafted starting batters.
#' @param df_pit A data frame of drafted starting pitchers.
#' @param bench_slots A named vector, the number of bench players drafted.
#'
#' @return A character vector of fangraphs_ids for top available bench players.
#' @noRd
top_avail_bench <- function(df_bat, df_pit, bench_slots) {
  if (bench_slots == 0) {
    return(NULL)
  }
  
  bat <- df_bat[, c("fangraphs_id", "pos", "zSUM", "drafted")]
  pit <- df_pit[, c("fangraphs_id", "pos", "zSUM", "drafted")]
  all <- rbind(bat, pit)
  all <- all[order(all$zSUM, decreasing = TRUE), ]
  
  bench <- find_top_avail(all, bench_slots)
  return(bench)
} 

#' Find the final ranking of all players (batters and pitchers combined)
#'
#' @param drafted_bat A data frame with drafted starting and bench batters 
#'                    identified.
#' @param drafted_pit A data frame with drafted starting and bench pitchers 
#'                    identified.
#'
#' @returns A character vector, rank ordered fangraphs_ids of drafted players.
#' @noRd
combined_rankings <- function(drafted_bat, drafted_pit) {
  b <- drafted_bat |>
    dplyr::filter(drafted == TRUE) |>
    dplyr::select(fangraphs_id, zSUM)
  
  p <- drafted_pit |>
    dplyr::filter(drafted == TRUE) |>
    dplyr::select(fangraphs_id, zSUM)
  
  all <- dplyr::bind_rows(b, p) |>
    dplyr::arrange(dplyr::desc(zSUM))
  
  ranks <- all$fangraphs_id
  return(ranks)
}