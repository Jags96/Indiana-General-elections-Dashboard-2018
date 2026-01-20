

source("utils.R")




DemoVSRepub_ui <- function(id,counties_vector,offices){
      ns <- NS(id)
      tagList(
      card(card_header("DEMO vs REPUB"),min_height = 500,plotOutput(ns("DEMO_REPUB_Bar_plot")),full_screen = TRUE),
      
      card(
      layout_column_wrap(
        card(card_header("Select Counties"),selectizeInput(
                                                           inputId = ns("SelectedCounty"),
                                                           label = "County:",
                                                           choices = counties_vector,
                                                           selected = c("MARION","JOHNSON","LAPORTE"),
                                                           multiple = TRUE,
                                                           ## important , this allowed the dropdown to appear out of card frame
                                                           options = list(
                                                             dropdownParent = "body",
                                                             maxItems = NULL)
                                                         )
            ),
        card(card_header("Select Offices"),selectizeInput(
                                                          inputId = ns("SelectedOffice"),
                                                          label = "Offices:",
                                                          choices = offices,
                                                          selected = c("US SENATE","US OFFICE","SECRETARY OF STATE","STATE TREASURER"),
                                                          multiple = TRUE,
                                                          options = list(
                                                            dropdownParent = "body",
                                                            maxItems = NULL)
                                                        )
            )
                    )
      )
      )

}


DemoVSRepub_server <- function(id,Demo_republic_votes_diff_office_static){
  moduleServer(id, function(input, output, session){
        
                Demo_republic_votes_diff_office_df <- reactive({
                  Demo_republic_votes_diff_office_static |>
                    filter(county_name %in% input$SelectedCounty,office %in% input$SelectedOffice) |>
                    group_by(office)|>
                    summarise(vote_diff = sum(vote_diff),.groups = "drop")
                })
                
                output$DEMO_REPUB_Bar_plot <- renderPlot({    
                  plot_BAR_DEMO_REP_OFFICE(Demo_republic_votes_diff_office_df())
                })
    
    
  })
    
}



