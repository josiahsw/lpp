
#' Draft starting (non-bench) batters
#' 
#' Simulates a draft. Identifies top players available at each slot by z-score
#' and marks them as drafted. Starting with main slots (1B, 2B, etc.), then marks
#' the next available MI and CI, then fills the remaining top players available 
#' at UT.
#'
#' @param df Data frame of batter projection z-scores
#' @param slots A named vector, batting slots and number to draft at each slot
#'    from lg_config
#' @return A data frame, with drafted column
#' @noRd
draft_starters <- function(df, slots, stat) {
  stat <- match.arg(stat, choices = c("bat", "pit"))
  
  df <- df[order(df$zSUM, decreasing = TRUE), ]
  df$drafted <- F
  
  if (stat == "bat") {
    stopifnot(
      all(c("C", "1B", "2B", "3B", "SS", "OF", "MI", "CI", "UT") %in% 
            names(slots)))
    
    main_slots <- slots[c("C", "1B", "2B", "3B", "SS", "OF")]
    
    drafted <- vector("list", length(main_slots))
    for (i in seq_along(main_slots)) {
      drafted[[i]] <- top_avail(df, main_slots[i])
    }
    
    main <- unlist(drafted)
    df <- mark_drafted_players(df, main)
    
    multi_slots <- slots[c("CI", "MI")]
    
    drafted <- vector("list", length(multi_slots))
    for (i in seq_along(multi_slots)) {
      drafted[[i]] <- top_avail(df, multi_slots[i])
    }
    
    multi <- unlist(drafted)
    df <- mark_drafted_players(df, multi)
    
    ut <- top_avail(df, slots["UT"])
    df <- mark_drafted_players(df, ut)
    
  } else {
    stopifnot(all(c("SP", "RP", "P") %in% names(slots)))
    
    main_slots <- slots[c("SP", "RP")]
    
    drafted <- vector("list", length(main_slots))
    for (i in seq_along(main_slots)) {
      drafted[[i]] <- top_avail(df, main_slots[i])
    }
    
    main <- unlist(drafted)
    df <- mark_drafted_players(df, main)
    
    p <- top_avail(df, slots["P"])
    df <- mark_drafted_players(df, p)
  }
  
  return(df)
}

# helpers -----------------------------------------------------------------

#' Identify top available players
#' 
#' A helper that returns a vector of the top available players at a slot by 
#' z-score. For example, for the slot "CI = 12" it will return a vector
#' of player ids of the top 12 un-drafted players eligible for the CI slot. 
#'
#' @param df A data frame of player projections
#' @param slot A named vector, where the name is the slot and the value is the 
#'    number of players drafted at that slot.
#'
#' @return A character vector of fangraphs_id of the top available players at a 
#'    slot.
#' @noRd
top_avail <- function(df, slot) {
  stopifnot(length(slot) == 1, !is.null(names(slot)))
  
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
    # included filter !drafted to be safe, but maybe not strictly needed
    df <- subset(df, !drafted & pos == names(slot))
  }
  
  fg_ids <- head(df$fangraphs_id, n = slot)
  return(fg_ids)
}

#' Mark drafted players
#'
#' @param df A data frame of player projections
#' @param drafted_players Character vector, fangraphs_ids of drafted players
#'
#' @return A data frame with drafted players marked T
#' @noRd
mark_drafted_players <- function(df, drafted_players) {
  df$drafted <- ifelse(df$fangraphs_id %in% drafted_players, T, df$drafted)
  return(df)
}

#' Identify top available bench players
#'
#' @param df_bat Data frame of drafted batters
#' @param df_pit Data frame of drafted pitchers
#' @param bench_slots A named vector of bench slots
#'
#' @return A character vector of fangraphs_ids for top available bench players
#' @noRd
top_avail_bench <- function(df_bat, df_pit, bench_slots) {
  if (bench_slots == 0) {
    return(NULL)
  }
  
  bat <- df_bat[, c("fangraphs_id", "pos", "zSUM", "drafted")]
  pit <- df_pit[, c("fangraphs_id", "pos", "zSUM", "drafted")]
  all <- rbind(bat, pit)
  all <- all[order(all$zSUM, decreasing = TRUE), ]
  
  bench <- top_avail(all, bench_slots)
  
  return(bench)
} 

combined_rankings <- function(bat, pit) {
  b <- bat %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::select(fangraphs_id, zSUM)
  
  p <- pit %>%
    dplyr::filter(drafted == TRUE) %>%
    dplyr::select(fangraphs_id, zSUM)
  
  all <- dplyr::bind_rows(b, p) %>%
    dplyr::arrange(desc(zSUM))
  
  ranks <- all$fangraphs_id
  return(ranks)
}

