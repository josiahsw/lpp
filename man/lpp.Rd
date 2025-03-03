% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lpp.R
\name{lpp}
\alias{lpp}
\title{Run Last Player Picked}
\usage{
lpp(
  bat = NULL,
  pit = NULL,
  keepers = NULL,
  lg = c("MLB", "AL", "NL"),
  teams = 12,
  budget = 260,
  min_bid = 1,
  bat_cat = c("HR", "R", "RBI", "SB", "AVG"),
  pit_cat = c("W", "SV", "SO", "ERA", "WHIP"),
  bat_pos = c(C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1, CI = 1, MI = 1, OF = 5, UT =
    1),
  pit_pos = c(SP = 5, RP = 3, P = 0),
  bench = 2,
  pos_adj = c("hold_harmless", "zero_out", "DH_to_1B", "simple", "bat_pit", "none")
)
}
\arguments{
\item{bat}{A data frame of batter projections from fangraphs.}

\item{pit}{A data frame of pitcher projections from fangraphs.}

\item{keepers}{A data frame of keepers and values. Defaults to NULL.}

\item{lg}{A string, the league type. One of "MLB" (the default), "AL", or
"NL".}

\item{teams}{A whole number greater than 0. The number of teams in the
league. Defaults to 12.}

\item{budget}{A numeric value greater than 0. The starting auction budget of
each team in the league. Defaults to 260.}

\item{min_bid}{A numeric value greater than or equal to 0. The minimum
bid per player. Defaults to 1.}

\item{bat_cat}{A character vector of league batting stat categories.
Available options are HR, R, RBI, SB, AVG, and OBP. The
default is c("HR", "R", "RBI", "SB", "AVG").}

\item{pit_cat}{A character vector of league pitching stat categories.
Available options are W, QS, WQS, SV, HLD, SVHLD, SO, ERA,
and WHIP. If quality starts are not included in the pitcher
projections expected QS will be automatically calculated based
on the Cup of Fantasy Joe xQS formula. The default is c("W",
"SV", "SO", "ERA", "WHIP").}

\item{bat_pos}{A named numeric vector where batting position is the name and
the number of batters drafted at that position per team is the
value. The default is c("C" = 1, "1B" = 1, "2B" = 1, "3B" = 1,
"SS" = 1, "CI" = 1, "MI" = 1, "OF" = 5, "UT" = 1). All
position names in the default vector must be given and all
values must be greater than or equal to 0.}

\item{pit_pos}{A named numeric vector where pitching position is the name and
the number of pitchers drafted at each position per team is
the value. The default is c("SP" = 5, "RP" = 3, "P" = 0). All
position names in the default vector must be given and all
values must be greater than or equal to 0.}

\item{bench}{A numeric value, the number of bench players drafted by each
team. Defaults to 2.}

\item{pos_adj}{A string, the position adjustment method. One of "simple",
"hold_harmless" (the default), "zero_out", "DH_to_1B",
"bat_pit", or "none".}
}
\value{
A list of length 2 containing a data frame of batter and pitcher
auction values.
}
\description{
Run Last Player Picked
}
