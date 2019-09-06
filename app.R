library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinythemes)
load("taxi.Rdata")

# Define UI for application that plots features of movies -----------
ui <- fluidPage(theme = shinytheme("united"),
  
  # Application title -----------------------------------------------
  titlePanel("NYC Green Taxi Data"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      
      # Create input buttons for x-axis of scatterplot
      
      selectInput("xvar", label = h3("X-Axis for Scatterplot"),
                   choices = list("Trip Distance" = "trip_distance",
                                  "Tolls Amount" = "tolls_amount",
                                  "Fare Amount" = "fare_amount"), 
                   selected = "Fare Amount"),
      hr(),

      
      # Select variable for datatable---------------------------------
      checkboxGroupInput(inputId = "selected",
                         label = "Filter Vendor IDs for Data Table:",
                         choices = c("1 : Creative Mobile Technologies" = "1", 
                                     "2 : Verifone Inc" = "2"),
                         selected = "1")
    ), 
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      plotOutput(outputId = "scatter"),
      br(),
      # Show datatable --------------------------------------------
      DT::dataTableOutput(outputId = "ridestable"),
      br()        # a little bit of visual separation
      
    )
  )
)

# Define server function required to create all the things ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected vendor types ------
  taxi_subset <- reactive({
    req(input$selected) # ensure availablity of value before proceeding
    filter(taxi, VendorID %in% input$selected)
  })
  
  output$scatter <- renderPlot({
    ggplot(taxi, aes_string(input$xvar, "tip_amount")) +
      geom_bin2d(bins = 50) + 
      xlim(0, (mean(taxi[[input$xvar]]) + 10*sd(taxi[[input$xvar]]))) +
      ylim(0, (mean(taxi$tip_amount)) + 10*sd(taxi$tip_amount)) +
      labs(x = toTitleCase(str_replace_all(input$xvar, '_', ' ')),
                           y = "Tip Amount",
           title = paste0("Distribution of ", 
                          toTitleCase(str_replace_all(input$xvar, '_', ' ')), 
                          " For NYC Green Taxi"))
  }) 

  # Create datatable object the output function is expecting --
  output$ridestable <- DT::renderDataTable(
      DT::datatable(data = taxi_subset()[, c(1, 8:13)], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
#

