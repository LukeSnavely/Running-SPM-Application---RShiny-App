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
library(DT)

# Reading in top running songs
songs <- read.csv("/Users/lukesnavely/Desktop/Capstone/Data/Top_100_Running_Songs.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Title
  titlePanel("Running Song Generator"),
  
  # Inputting height
  numericInput("height", "Height (inches):",
               value = 70, min = 48, max = 84),
  
  # Inputting age
  numericInput("age", "Age (Years):",
               value = 20, min = 10, max = 100),
  
  # Minutes and Seconds inputs
  tags$label("Pace Input", class = "control-label"),
  fluidRow(
    column(2, numericInput("MINUTES", label = NULL, value = 8, min = 5, max = 15, step = 1)), # Minutes box
    column(1, p("min", style = "margin-top: 8px; font-weight: bold;")), # Minutes title
    column(2, numericInput("SECONDS", label = NULL, value = 0, min = 0, max = 59, step = 15)), # Seconds box
    column(1, p("sec", style = "margin-top: 8px; font-weight: bold;")) # Seconds title
  ),
  
  # Button to generate results
  actionButton("generate", "Generate Running Songs"),

  # Display estimated SPM
  textOutput("estimated_spm"),
  
  # Showing matched songs to calculated cadence
  dataTableOutput("matching_songs"),
  
  # Spotify player
  uiOutput("spotify_player")
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
  
  # This is the estimated cadence
  output$estimated_spm <- renderText({
    paste(
      "Estimated cadence: ",
      round(estimated_spm()),
      " Steps per Minute"
    )
  })
  
  # Filtering for songs that match the BPM
  matching_songs <- eventReactive(input$generate, {
    
    # BPM of the song
    bpm <- 203.056 +
      0.193 * input$age -
      44.242 * height_m() +
      3.067 * speed_kmh()

    songs |>
      dplyr::filter(Tempo <= bpm + 5, Tempo >= bpm - 5) |>
      dplyr::select(
        Track.URI,
        Track.Name,
        Artist.Name.s.,
        Tempo
      )
  })
  
  # Create Spotify player
  output$spotify_player <- renderUI({
    
    req(input$selected_song)
    
    # Convert Spotify URI:
    # spotify:track:6epn3r7S14KUqlReYr77hA
    # into:
    # 6epn3r7S14KUqlReYr77hA
    track_id <- sub(
      "spotify:track:",
      "",
      input$selected_song
    )
    
    tags$iframe(
      src = paste0(
        "https://open.spotify.com/embed/track/",
        track_id
      ),
      width = "100%",
      height = "152",
      frameborder = "0",
      allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading = "lazy",
      style = "border-radius: 12px; border: none;"
    )
  })
  
  # Display table
  output$matching_songs <- renderDataTable({
    
    results <- matching_songs()
    
    # Create Play button for each song
    results$Play <- paste0(
      '<button class="spotify-button" ',
      'onclick="Shiny.setInputValue(\'selected_song\', \'',
      results$Track.URI,
      '\', {priority: \'event\'})">',
      '▶ Play',
      '</button>'
    )
    
    results |>
      dplyr::select(
        Track.Name,
        Artist.Name.s.,
        Tempo,
        Play
      )
    
  },
  escape = FALSE,
  selection = "none",
  options = list(
    pageLength = 10
  ))

}


# Run the application 
shinyApp(ui = ui, server = server)
