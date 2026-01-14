library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(tidyr)
library(plotly)

source("utils.R")


## If the df is Continuously integrated, then we will need to put it into reactive method
df<-read.csv("2018-in-precinct-general.csv",header = TRUE)
df$party_simplified <- ifelse(is.na(df$party_simplified) | (df$party_simplified == ""),"OTHER",df$party_simplified)
counties <- unique(df$county_name) 
parties <- unique(df$party_simplified)
offices <- unique(df$office)


## its better to store already group by-ed DF as well, since its quick filter 
county_party_groupby <- df |> group_by(county_name, party_simplified)|> summarise(total_votes = sum(votes),.groups = "drop")
Demo_republic_votes_diff_df <- Demo_republic_votes_diff(df) ## can be used as global object
County_total_votes_IN_MAP_df <- County_total_votes_IN_MAP(df) 
Demo_republic_votes_diff_office_static <- df |> 
  filter(party_simplified %in% c("DEMOCRAT","REPUBLICAN"))|>
  group_by(office,party_simplified,county_name) |> 
  summarise(total_votes = sum(votes),.groups = "drop") |>  
  pivot_wider( names_from  = party_simplified,values_from = total_votes,values_fill = 0) |> ## pivoting only party simplified and total_votes 
  mutate(vote_diff = DEMOCRAT - REPUBLICAN) 

## I am not using Modularity here, since I am out of time
## Basically, it has three tab-style panels, with each of the tab having its filter options 
### For better experience plotly can be used in future

ui <- fluidPage(

    # Application title
    titlePanel("Indiana General Elections Results 2018"),
    
    
    tabsetPanel(
      tabPanel(
          "HEATMAP: Indiana Map",
          mainPanel(
            h4("This plot shows county-wise Total Votes"),
            p("Note: Total Votes include all types of votes, like US Senate etc,"),
            fluidRow(
              column(6,plotlyOutput("INDIANA_COUNTY_MAP_TOTAL_VOTES")),
              column(6,plotlyOutput("INDIANA_COUNTY_MAP_DEMO_REPUBLIC"))
            )
        )
      ),

      tabPanel(
        "BAR PLOT : DEMOCRAT vs REPUBLICAN",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "SelectedCounty",
              label = "County",
              choices = counties, 
              selected = "MARION",
              multiple = TRUE
            ),
            selectInput(
              inputId = "SelectedOffice",
              label = "Office",
              choices = offices,
              selected = c("US SENATE","US OFFICE","SECRETARY OF STATE","STATE TREASURER"),
              multiple = TRUE
            )
          ),
          mainPanel(
            h3("This plot, show difference between Votes between DEMOCRAT and REPUBLICAN"),
            plotOutput("DEMO_REPUB_Bar_plot"),
            p("Blue indicated Democrats recieved more votes than Republicans,\n and Red indicates Republican Recieved more votes")
          )
        )
      ),
      tabPanel(
        "BAR PLOT: Party Votes per County",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "SelectedCounty_summaryplot",
              label = "County",
              choices = counties, 
              selected = c("MARION","JOHNSON","LAPORTE"),
              multiple = TRUE
            ),
            selectInput(
              inputId = "SelectedParty_summaryplot",
              label = "Party",
              choices = parties, 
              selected = "DEMOCRAT",
              multiple = TRUE
            )
          ),
          mainPanel(
            h4("Select Counties to compare between Parties Votes"),
            plotOutput("CountyWise_plot"),
            p("\n\n"),
            hr(),
            h4("Select parties to compare between County-wise Votes"),
            plotOutput("PartyWise_plot")
          )
        )
      )
    )

)


server <- function(input, output) {
  ## for CI data, If we get from API , I need to keep updating df periodically lets say!!! 
  ## That's the reason behind I am restarting pipeline from the very df (in my utils.R file functions)
  
  # ------- All these methods use Global Object above ------------------------------
    filtered_county_votes_df <- reactive(
     { county_party_groupby |> filter(county_name %in% input$SelectedCounty_summaryplot)}
    )
    
    filtered_party_county_df <- reactive({
      county_party_groupby |> filter(county_name %in% input$SelectedCounty_summaryplot,party_simplified %in% input$SelectedParty_summaryplot)
    })
    

    
    
    ## county wise plot
    output$CountyWise_plot <- renderPlot({
      ggplot(filtered_county_votes_df(), mapping = aes(y = total_votes,x = party_simplified)) + geom_col() + labs(
        title = "Party-wise Votes distribution for all selected County/ies",
        y = "Votes",
        x = "Parties"
      ) + theme_minimal()
    })
    
    ## Overall Plot
    output$PartyWise_plot <- renderPlot({
      filtered_party_county_df() |> ggplot(mapping = aes(y = total_votes,x = county_name)) + geom_col() + labs(
        title = "County-wise Votes for selected Party/ies",
        y = "Votes",
        x = "Counties"
      ) + theme_minimal()
    })
    
    
    
  # -----------------------------------------------------
    ### DEMO vs REPUB OFFICE BAR PLOT
    
#     Demo_republic_votes_diff_office_df <- reactive({
#      Demo_republic_votes_diff_office(df,input$SelectedCounty,input$SelectedOffice)
#    }) ## use this if df is dynamic
    
     Demo_republic_votes_diff_office_df <- reactive({
       Demo_republic_votes_diff_office_static |>
         filter(county_name %in% input$SelectedCounty,office %in% input$SelectedOffice) |>
         group_by(office)|>
         summarise(vote_diff = sum(vote_diff),.groups = "drop")
       })
    
      output$DEMO_REPUB_Bar_plot <- renderPlot({    
        plot_BAR_DEMO_REP_OFFICE(Demo_republic_votes_diff_office_df())
  })
      
      
  # --------------------------------------------------
      ## Indiana map total votes
#      County_total_votes_IN_MAP_df <- reactive({
 #       County_total_votes_IN_MAP(df)
  #    })
      
      ## its plot
      output$INDIANA_COUNTY_MAP_TOTAL_VOTES <- renderPlotly({
        ggplotly(plot_IN_MAP_TOTAL_VOTES(County_total_votes_IN_MAP_df, indiana_counties))
      })
      
  # --------------------------------------------------    
      ## for IN MAP DEMO vs REP
 #     Demo_republic_votes_diff_df <- reactive({
#        Demo_republic_votes_diff(df)
#        }) ## this is for CI df

      ## its plot
      output$INDIANA_COUNTY_MAP_DEMO_REPUBLIC <- renderPlotly({
        ggplotly(plot_IN_MAP_REP_DEMO(Demo_republic_votes_diff_df, indiana_counties))
      })
      
  # --------------------------------------------------
    
}

# Run the application 
shinyApp(ui = ui, server = server)
