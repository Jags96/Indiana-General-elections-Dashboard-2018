
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(tidyr)
library(plotly)
library(bslib)


source("utils.R")


IndianaMap_ui <- function(id){
    ns = NS(id)
    
    card(
      title = "Welcome to Indiana General Elections Results dashboard!!",
      layout_column_wrap(
        value_box(title = "Total Votes across all offices",
                  value = textOutput(ns("TotalVotes")),
                  theme = "green",
                  ),
        value_box(title = "Total Democrat Votes across all offices",
                  value = textOutput(ns("TotalVotesDEMOCRATs")),
                  theme = DEM_THEME,
        ),
        value_box(title = "Total Republican Votes across all offices",
                  value = textOutput(ns("TotalVotesREPUBLICANs")),
                  theme = REP_THEME,
        )
      ),
      
      layout_column_wrap(
        card(title = "Total Votes in Indiana",plotlyOutput(ns("INDIANA_COUNTY_MAP_TOTAL_VOTES")),full_screen = TRUE),
        card(title = "DEMOCRAT - REPUBLICAN vote difference",plotlyOutput(ns("INDIANA_COUNTY_MAP_DEMO_REPUBLIC")),full_screen = TRUE)
      )
    )
}



IndianaMap_server <- function(id,Total_Votes_per_Party,County_total_votes_IN_MAP_df,Demo_republic_votes_diff_df,indiana_counties){
  moduleServer(id, function(input, output, session){
    ## Value Box counts
    output$TotalVotesDEMOCRATs <- reactive({
      Total_Votes_per_Party[Total_Votes_per_Party$party_simplified == "DEMOCRAT",]$total_votes
    })
    ## for Repub
    output$TotalVotesREPUBLICANs <- renderText({
      Total_Votes_per_Party[Total_Votes_per_Party$party_simplified == "REPUBLICAN",]$total_votes
    })
    ## for Total across all offices and parties
    output$TotalVotes <- renderText({
      sum(Total_Votes_per_Party$total_votes)
    })
    
    # --------------------------------------------------
    ## Indiana map total votes
    
    ## its plot
    output$INDIANA_COUNTY_MAP_TOTAL_VOTES <- renderPlotly({
      ggplotly(plot_IN_MAP_TOTAL_VOTES(County_total_votes_IN_MAP_df, indiana_counties))
    }) |>
      bindCache("static_map_0",id)
    
    # --------------------------------------------------    
    ## for IN MAP DEMO vs REP
    
    output$INDIANA_COUNTY_MAP_DEMO_REPUBLIC <- renderPlotly({
      ggplotly(plot_IN_MAP_REP_DEMO(Demo_republic_votes_diff_df, indiana_counties))
    }) |> 
      bindCache("static_map",id)
    
  })
}