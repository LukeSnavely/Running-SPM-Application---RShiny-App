#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib) # For themes

# Making a pace table
# pace_table <- data.frame(
#   total_seconds = seq(240, 1200, by = 15) # 4 to 20 minutes by 15 seconds
#   ideal_bpm = c()
# )

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Title
  titlePanel("Running Song Generator"),
  
  # Inputting height
  numericInput("height", "Height (inches):",
               value = 0, min = 48, max = 84),
  
  # Inputting age
  numericInput("age", "Age (Years):",
               value = 0, min = 10, max = 100),
  
  # Minutes and Seconds inputs
  tags$label("Pace Input", class = "control-label"),
  fluidRow(
    column(2, numericInput("MINUTES", label = NULL, value = 0, min = 4, max = 20, step = 1)), # Minutes box
    column(1, p("min", style = "margin-top: 8px; font-weight: bold;")), # Minutes title
    column(2, numericInput("SECONDS", label = NULL, value = 0, min = 0, max = 59, step = 15)),
    column(1, p("sec", style = "margin-top: 8px; font-weight: bold;"))
  ),
  
  # Button to generate results
  actionButton("generate", "Generate Running Songs"),

  # Display estimated SPM
  textOutput("estimated_spm")
)

server <- function(input, output, session) {
  # This converts the inputted minutes and seconds into total pace time
  # Convert pace to seconds
  pace_seconds <- reactive({
    input$MINUTES * 60 + input$SECONDS
  })
  
  # Convert pace to mph
  speed_mph <- reactive({
    3600 / pace_seconds() # 60 seconds * 60 minutes in an hour --> gets how many seconds to run 1 mile
  })
  
  # Converting pace to km/h (that's what the study uses)
  speed_kmh <- reactive({
    speed_mph() * 1.60934 # 1 mile = 1.60934 km
  })
  
  # Convert height from inches to meters
  height_m <- reactive({
    input$height * 0.0254 # 1 inch = 0.0254 meters
  })
  
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC10588426/
  # This article has a linear regression model to calculate step length. It has an adjusted R^2 of 0.88, so pretty reliable.
  estimated_spm <- eventReactive(input$generate, { # Will only generate results if the button is pressed
    203.056 +
      0.193 * input$age -
      44.242 * height_m() +
      3.067 * speed_kmh()
  })
  
  output$estimated_spm <- renderText({
    paste(
      "Estimated cadence: ",
      round(estimated_spm()),
      " Steps per Minute"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
