library(shiny)
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)


### CONFIGS VALUES

DEM_COLOR <- "#1E3A8A"
REP_COLOR <- "#B22222"

## for Value BOX themes
DEM_THEME <- value_box_theme(bg = "#1E3A8A", fg = "#FFFFFF")
REP_THEME <- value_box_theme(bg = "#B22222", fg = "#FFFFFF")


UNKNOWN_COLOR <- "grey90"
#COUNTY_NAMES <- can be hard coded, since it static values, but  I will use unique(df$county_names) in app.R


### INDIANA COUNTY MAP - Static layout
options(tigris_use_cache = TRUE)

indiana_counties <- counties(
  state = "IN",
  cb = TRUE,        
  year = 2022
) |> mutate(county_fips = as.integer(GEOID))



###----------- TRANSFORMATIONS METHODS --------------------------------------------

## for CI data, If we get from API , I need to keep updating df !!! 
## That's the reason behind why I am restarting pipeline from the very df (in my utils.R file functions)
## We can get little smart and use

## For county-wise votes for all parties
County_wise_votes_filtered <- function(df,SelectedCounty){
  df |> group_by(county_name, party_simplified)|> summarise(total_votes = sum(votes),.groups = "drop") |> filter(county_name %in% SelectedCounty)
}

## shortlisted county for Demo and Repub - for Bar Plot
## This is for changing-df"
Demo_republic_votes_diff_office <- function(df, SelectedCounty,SelectedOffice){
  
  df |> filter(party_simplified %in% c("DEMOCRAT","REPUBLICAN"), county_name %in% SelectedCounty, office %in% SelectedOffice) |>
    group_by(office,party_simplified) |> summarise(total_votes = sum(votes),.groups = "drop") |>  
    pivot_wider( names_from  = party_simplified,values_from = total_votes,values_fill = 0) |> ## pivoting only party simplified and total_votes 
    mutate(vote_diff = DEMOCRAT - REPUBLICAN) 
}
## this is for static df
Demo_republic_votes_diff_office_static <- function(df, SelectedCounty,SelectedOffice){
  

}





## IN Map Demo vs repub
Demo_republic_votes_diff <- function(df){
  df |> 
    filter(party_simplified %in% c("DEMOCRAT","REPUBLICAN")) |>
    group_by(county_fips,party_simplified) |> 
    summarise(total_votes = sum(votes),.groups = "drop") |> 
    pivot_wider(names_from = party_simplified, values_from = total_votes,values_fill = 0) |> 
    mutate(vote_diff = DEMOCRAT - REPUBLICAN)
}

## IN Map total votes - all votes including all offices
County_total_votes_IN_MAP <- function(df){
  df |>
    group_by(county_fips) |>
    summarise(total_votes = sum(votes)) 
}


### PLOTTINGS METHODS



## INDIANA MAP PLOT

plot_IN_MAP_TOTAL_VOTES <- function(County_total_votes_IN_MAP_df, indiana_counties){

  indiana_counties |> left_join(County_total_votes_IN_MAP_df, by = "county_fips") |>
  ggplot() +
  geom_sf(aes(fill = total_votes), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Total Votes by County (Indiana)",
    fill = "Votes"
  ) +
  theme_void()
}

## INDIANA MAP PLOT - DEMO vs REPUB

plot_IN_MAP_REP_DEMO <- function(Demo_republic_votes_diff_df, indiana_counties){
    indiana_counties |> left_join(Demo_republic_votes_diff_df, by  = "county_fips") |>
    ggplot() +
    geom_sf(aes(fill = vote_diff), color = "white", linewidth = 0.2) +
    scale_fill_gradient2(
      low = REP_COLOR,
      mid = "white",
      high = DEM_COLOR, 
      midpoint = 0,
      na.value = UNKNOWN_COLOR,
      name = "Vote Margin\n(DEM − REP)"
    ) +
    theme_void() +
    labs(title = "Indiana County Vote Margin")
}

## BAR PLOT - DEMO vs REP OFFICE & COUNTY filtered

#Demo_republic_votes_diff_office

plot_BAR_DEMO_REP_OFFICE <- 
  function(Demo_republic_votes_diff_office_df,
           title ="Democrat vs Republican Vote Difference by Office" ){
    
    Demo_republic_votes_diff_office_df$is_DEMO_lead <- factor(Demo_republic_votes_diff_office_df$vote_diff > 0, levels = c(TRUE, FALSE) )
      
    Demo_republic_votes_diff_office_df |> 
    ggplot( aes(x = office, y = vote_diff, fill = is_DEMO_lead)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title,
      x = "Office",
      y = "Vote Difference (DEM - REP)"
    ) +
    scale_fill_manual(values = c("FALSE" = REP_COLOR,"TRUE" = DEM_COLOR), guide = "none") + theme_minimal()
}




