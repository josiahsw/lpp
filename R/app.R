

# load packages -----------------------------------------------------------
library(shiny)
library(bslib)
library(DT)


# import projections ------------------------------------------------------
bat <- reactive(gardy::fbdb_tbl_max("projections_batters"))
# pit <- gardy::fbdb_tbl_max("projections_pitchers")

# params ------------------------------------------------------------------
# ui
LG_PRESETS <- "BEER"
PLAYER_POOL <- c("MLB", "NL", "AL")
CAT_BAT <- c("HR", "R", "RBI", "SB", "AVG", "OBP")
CAT_PIT <- c("W", "QS", "W+QS", "SV", "HLD", "SV+HLD", "SO", "ERA", "WHIP")
DEFAULT_CAT_BAT <- c("HR", "R", "RBI", "SB", "OBP")
DEFAULT_CAT_PIT <- c("W+QS", "SV+HLD", "SO", "ERA", "WHIP")
CALC_PRESETS <- c("Pre-season", "Rest of Season", "3-Year")
# TYPE <- c("atc", "atc_ros", "depthcharts", "depthcharts_ros", "steamer", 
#           "steamer_ros", "steamer_update", "steamer600", "steamer600_update",
#           "thebat", "thebat_ros", "thebatx", "thebatx_ros", "zips", "zips_ros",
#           "zips_update", "zips_yr2", "zips_yr3")
TYPE <- unique(bat()$type)
POS_FILTER <- c("Batters", "Pitchers", "Starters", "Relievers")

# server
POS_PRIORITY <- c(
  "C"  = 1, 
  "SS" = 2, 
  "2B" = 3, 
  "3B" = 4, 
  "OF" = 5, 
  "1B" = 6, 
  "DH" = 7
)

# ui ---------------------------------------------------------------------
ui <- page_fluid(
  titlePanel("Last Player Picked"),
  layout_columns(
    card(
      card_header("Roto Categories"),
      layout_columns(
        column(6,
               checkboxGroupInput("catbat", label = "Batting", choices = CAT_BAT, selected = DEFAULT_CAT_BAT)
        ),
        column(6,
               checkboxGroupInput("catpit", label = "Pitching", choices = CAT_PIT, selected = DEFAULT_CAT_PIT)
        )
      )
    ),
    card(
      card_header("Positions"),
      layout_columns(
        column(4,
               numericInput("c", label = "C", value = 1, min = 0, max = 2),
               numericInput("first", label = "1B", value = 1, min = 0, max = 1),
               numericInput("second", label = "2B", value = 1, min = 0, max = 1),
               numericInput("ss", label = "SS", value = 1, min = 0, max = 1),
               numericInput("third", label = "3B", value = 1, min = 0, max = 1)
        ),
        column(4,
               numericInput("of", label = "OF", value = 5, min = 0, max = 5),
               numericInput("mi", label = "MI", value = 1, min = 0, max = 1),
               numericInput("ci", label = "CI", value = 1 , min = 0, max = 1),
               numericInput("ut", label = "UT", value = 1, min = 0, max = 2)
        ),
        column(4,
               
               numericInput("sp", label = "SP", value = 0, min = 0, max = 10),
               numericInput("rp", label = "RP", value = 0, min = 0, max = 10),
               numericInput("p", label = "P", value = 9, min = 0, max = 10),
               numericInput("bench", label = "Bench", value = 4, min = 0, max = 10)
        )
      )
    ),
    card(
      card_header("League Settings"),
      selectInput("lgpre", label = "Presets", choices = LG_PRESETS, selected = "BEER"),
      selectInput("lgmlb", label = "League", choices = LG_MLB, selected = "MLB"),
      numericInput("team", label = "Team", value = 12,  min = 10, max = 15),
      numericInput("budget", label = "Budget",  value = 260, min = 0,  max = 260, step = 5),
      numericInput("minbid", label = "Min Bid", value = 1,   min = 0,  max = 5)
    ),
    card(
      card_header("Calculator Settings"),
      selectInput("calcpre", label = "Presets", choices = CALC_PRESETS, selected = "Rest of Season"),
      selectInput("type", label = "Projection", choices = TYPE, selected = "atc_ros"),
      numericInput("batsplit", label = "Bat Split %", value = NULL, min = 0, max = 100),
      checkboxInput("keepers", label = "Keepers", value = FALSE),
      actionButton("calculate", "Calculate")
    ),
    card(
      card_header("Auction Values"),
      DTOutput("bat")
    ),
    card(
      card_header("Table Controls"),
      selectInput("pos", label = "Position Filter", choices = POS_FILTER, selected = "Batters"),
      actionButton("save", "Save to Database")
    ),
    col_widths = c(3, 5, 2, 2, 10, 2)
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  # reactive values
  teams <- reactiveVal(input$teams)
  budget <- reactiveVal(input$budget)
  min_bid <- reactiveVal(input$minbid)
  lg_mlb <- reactiveVal(input$lgmlb) 
  cat_bat <- reactiveVal(input$catbat)
  cat_pit <- reactiveVal(input$catpit)
  batters <- reactiveVal(c(
    "C"  = input$c, 
    "1B" = input$first, 
    "2B" = input$second,
    "3B" = input$third, 
    "SS" = input$ss, 
    "CI" = input$ci, 
    "MI" = input$mi, 
    "OF" = input$of, 
    "UT" = input$ut
    )
  )
  pitchers <- reactiveVal(c("SP" = input$sp, "RP" = input$rp, "P" = input$p))
  bench  <-  reactiveVal(c("bench" = input$bench))
  type <-  reactiveVal(input$type)
  batsplit <-  reactiveVal(input$batsplit)
  keepers <-  reactiveVal(input$keepers)
  
  # reactive expressions
  slots_bat <- reactive(batters() * teams())
  slots_pit <- reactive(pitchers() * teams())
  slots_bench <- reactive(bench() * teams())
  
  pop_bat <- reactive(sum(bat_slots()))
  pop_pit <- reactive(sum(pit_slots()))
  
  if (keepers() == FALSE) {
    keep <- NULL
  } else {
    keep <- reactive(gardy::fbdb_tbl("draft_results"))
  }
  
  # on click
  output$bat <- eventReactive(input$calculate, {DT::DTOutput(bat())})
}

shinyApp(ui, server)
