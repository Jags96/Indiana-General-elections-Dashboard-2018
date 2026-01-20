
source("utils.R")

summary_ui <- function(id){
  ns <- NS(id)
  
  layout_columns(
    card(
      card(card_header("Party-wise per all selected Counties Plots") ,plotOutput(ns("PartyWise_plot")),full_screen = TRUE),
      card(card_header("County-wise per all selected Parties Plots") ,plotOutput(ns("CountyWise_plot")),full_screen = TRUE)),
    card(
      card(card_header("Choose Parties:"),checkboxGroupInput(
        inputId = ns("SelectedParty_summaryplot"),
        label = "Party:",
        choices = parties_map, 
        selected = "DEMOCRAT"),
        helpText("Select parties to compare between County-wise Votes") ),# checkBoxGroupInput
      
      card(card_header("Choose Counties:"),selectizeInput(
              inputId = ns("SelectedCounty_summaryplot"),
              label = "County",
              choices = counties_vector,
              selected = c("MARION","JOHNSON","LAPORTE"),
              multiple = TRUE,
              ## important , this allowed the dropdown to appear out of card frame
              options = list(
                dropdownParent = "body",
                maxItems = NULL
              )
        
      )) # SelectizeInput
    ),
    col_widths = c(8,4)
  )
}
 



summary_server <- function(id,county_party_groupby){
  moduleServer(id,
               function(input, output, session){
                 ## There are input 
                 filtered_county_votes_df <- reactive({
                   input$SelectedCounty_summaryplot
                   county_party_groupby |> filter(county_name %in% input$SelectedCounty_summaryplot)
                   
                 }) 
                 
                 
                 filtered_party_county_df <- reactive({county_party_groupby |> 
                     filter(county_name %in% input$SelectedCounty_summaryplot,party_simplified %in% input$SelectedParty_summaryplot)})
                 
                 
                 ## these are plots
                 output$PartyWise_plot <- renderPlot({
                   ggplot(filtered_county_votes_df(), mapping = aes(y = total_votes,x = party_simplified)) + geom_col() + labs(
                         title = "Party-wise Votes distribution for all selected County/ies",
                         y = "Votes",
                         x = "Parties"
                       ) + theme_minimal()
                 })
                   
                 output$CountyWise_plot <- renderPlot({
                   filtered_party_county_df() |> ggplot(mapping = aes(y = total_votes,x = county_name)) + geom_col() + labs(
                     title = "County-wise Votes for selected Party/ies",
                     y = "Votes",
                     x = "Counties"
                   ) + theme_minimal()
                 })
               }
               
               
               )
}
