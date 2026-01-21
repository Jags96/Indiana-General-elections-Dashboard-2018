library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(tidyr)
library(plotly)
library(bslib)

source("utils.R")
source("R/IndianaMap.R")
source("R/SummaryPlots.R")
source("R/DemoVSRepub_Offices.R")


df <- read.csv("2018-in-precinct-general.csv", header = TRUE)
df$party_simplified <- ifelse(is.na(df$party_simplified) | (df$party_simplified == ""), "OTHER", df$party_simplified)
counties_vector <- unique(df$county_name)
parties <- unique(df$party_simplified)
parties_map <- setNames(parties, parties)
offices <- unique(df$office)


## its better to store already group by-ed DF as well, since its quick filter
county_party_groupby <- df |>
  group_by(county_name, party_simplified) |>
  summarise(total_votes = sum(votes), .groups = "drop")
Demo_republic_votes_diff_df <- Demo_republic_votes_diff(df) ## can be used as global object
County_total_votes_IN_MAP_df <- County_total_votes_IN_MAP(df)
Demo_republic_votes_diff_office_static <- df |>
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) |>
  group_by(office, party_simplified, county_name) |>
  summarise(total_votes = sum(votes), .groups = "drop") |>
  pivot_wider(names_from = party_simplified, values_from = total_votes, values_fill = 0) |> ## pivoting only party simplified and total_votes
  mutate(vote_diff = DEMOCRAT - REPUBLICAN)

#### Total Votes and Total Demo and total REpublicans VOtes
Total_Votes_per_Party <- df |>
  group_by(party_simplified) |>
  summarise(total_votes = sum(votes), .groups = "drop")


ui <- page_navbar(
  title = "Indiana General Elections Results 2018",
  theme = bs_theme(version = 5, bootswatch = "lux"),
  nav_panel(
    title = "Indiana Map",
    IndianaMap_ui(id = "IndianaMAP_2018") ##  IndianaMap_ui module call
  ),
  nav_panel(
    title = "Democrat vs Republican",
    DemoVSRepub_ui(id = "DemoVSRepub2018", counties_vector = counties_vector, offices = offices)
  ),
  nav_panel(
    title = "Summary Plots",
    summary_ui(id = "SummaryPanel", parties_map = parties_map, counties_vector = counties_vector)
  )
)


server <- function(input, output) {
  # ------- Summary Plots ------------------------------

  summary_server(
    id = "SummaryPanel",
    county_party_groupby = county_party_groupby
  )

  # -----------------------------------------------------
  ### DEMO vs REPUB OFFICE BAR PLOT

  DemoVSRepub_server(
    id = "DemoVSRepub2018",
    Demo_republic_votes_diff_office_static = Demo_republic_votes_diff_office_static
  )

  ## ------ Indiana Map Panel module call

  IndianaMap_server(
    id = "IndianaMAP_2018",
    Total_Votes_per_Party = Total_Votes_per_Party,
    County_total_votes_IN_MAP_df = County_total_votes_IN_MAP_df,
    Demo_republic_votes_diff_df = Demo_republic_votes_diff_df,
    indiana_counties = indiana_counties
  )
}

# Run the application
shinyApp(ui = ui, server = server)
