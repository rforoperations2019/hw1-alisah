library(shiny)
library(ggplot2)
library(DT)
library(data.table)
library(stringr)
library(dplyr)
library(tools)
library(shinythemes)
load("taxi.Rdata")

# Define UI for application that plots features of movies -----------
ui <- fluidPage(theme = shinytheme("united"), 
                downloadButton("downloadData", "Download Taxi Data"),
  
  # Application title -----------------------------------------------
  titlePanel("NYC Green Taxi Data"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      

      hr(),
      br(), 
      br(),  #Adding breaks to align inputs closely with appropriate visualizations.
      br(),     #I'll do this in a more sophisticated manner next time.
      br(), 
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(), 
      br(),
      br(),
      br(),
      br(),
      hr(),
      
      # Create input buttons for x-axis of scatterplot
      
      selectInput("xvar", label = h4("X-Axis for Heat Map"),
                  choices = list("Trip Distance" = "trip_distance",
                                 "Tolls Amount" = "tolls_amount",
                                 "Fare Amount" = "fare_amount"), 
                  selected = "Fare Amount"),
      br(),
      br(),
      br(), 
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(),
      
      # Set range on passenger counts for boxplots ------------------------
      radioButtons(inputId = "box", 
                  label = h4("Choose a boxplot input:"), 
                  choices = c("Trip Distance" = "trip_distance",
                              "Fare Amount" = "fare_amount",
                              "Tip Amount" = "tip_amount")),
      
      hr(),
      br(),
      br(),
      br(),
      
      # Select range of passengers ----------------------------------------------------
      sliderInput(inputId = "range", 
                   label = "Number of passengers for boxplot and data table:", 
                   min = 1, max = 8, 
                   value = c(1,5)),
      br(),
      br(),
      br(),
      br(),
      hr(),
      

      
      # Select variable for datatable---------------------------------
      checkboxGroupInput(inputId = "selected",
                         label = h4("Filter Vendor IDs for Data Table:"),
                         choices = c("1 : Creative Mobile Technologies" = "1", 
                                     "2 : Verifone Inc" = "2"),
                         selected = "1")
    ), 
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      #Show number of passengers histogram
      plotOutput(outputId = "hist"),
      br(),
      
      # Show density plot for x value and tip amount
      plotOutput(outputId = "statter"),
      br(), 
      
      # Show boxplot
      plotOutput(outputId = 'box'),
      br(),
      

      # Show datatable --------------------------------------------
      DT::dataTableOutput(outputId = "ridestable"),
      br()       # a little bit of visual separation
    
    )
  )
)

# Define server function required to create all the things ---------
server <- function(input, output, session) {
  
  # Update maximum number of passengers in the data table
  observe({
    updateNumericInput(session, 
                       inputId = "range",
                       value = min(1, 8),
                       max = 8)
  })
  
  # Create a subset of data filtering for selected vendor types ------
  taxi_subset <- reactive({
    req(input$range)
    req(input$selected) # ensure availablity of value before proceeding
    filter(taxi, VendorID %in% input$selected)
    filter(taxi, passenger_count %in% input$range)
  })
  
  # Create a plot of our independent variable and tip amount -----------
  output$statter <- renderPlot({
    ggplot(taxi, aes_string(input$xvar, "tip_amount")) +
      geom_bin2d(bins = 50) + 
      xlim(0, (mean(taxi[[input$xvar]]) + 10*sd(taxi[[input$xvar]]))) +
      ylim(0, (mean(taxi$tip_amount)) + 10*sd(taxi$tip_amount)) +
      labs(x = toTitleCase(str_replace_all(input$xvar, '_', ' ')),
                           y = "Tip Amount",
           title = paste0("Distribution of ", 
                          toTitleCase(str_replace_all(input$xvar, '_', ' ')), 
                          " for NYC Green Taxi"))
  }) 
  
  # Histogram of number of passengers--------------
  output$hist <- renderPlot({
    ggplot(taxi, aes_string("passenger_count")) +
      geom_histogram(binwidth = 1) +
      xlim(1,7) +
      ylim(0, 10000) +
      labs(x = "Number of Passengers",
           y = "Count")
  })
  
  # Boxplot of user selected variable--------------
  output$box <- renderPlot({
    ggplot(taxi, aes_string(x = "passenger_count", y =input$box, group = "passenger_count")) +
      geom_boxplot() +
      xlim(input$range) +
      ylim(0, (mean(taxi[[input$box]])) + sd(taxi[[input$box]])) +
      labs(x = "Passenger Count",
        y = toTitleCase(str_replace_all(input$box, '_', ' ')))
  })

  # Create datatable object the output function is expecting --
  output$ridestable <- DT::renderDataTable(
      DT::datatable(data = taxi_subset()[, c(1, 8:13)], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    )
  output$downloadData <- downloadHandler(
      filename = "Taxi_Data.csv", 
      content = function(file) {
        write.csv(taxi, file)
      }, contentType = "txt/csv") 
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
#

