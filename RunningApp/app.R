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
songs <- read.csv("/Users/lukesnavely/Desktop/Capstone/Running Application/RunningApp/Potential_songs.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#121212",# Dark background
    fg = "#FFFFFF", # White text
    primary = "#1DB954", # Spotify green
    secondary = "#1DB954"
  ),
  
  # Custom HTML
  tags$head(
    tags$style(HTML("
      .btn-custom {
        background-color: #1DB954;
        color: black;
        font-size: 20px;
        font-weight: bold;
        border: none;
      }
      
      .btn-custom:hover {
        background-color: #1AA34A;
        color: white;
      }
      
      h2 {
        color: white;
        font-size: 40px;
        font-weight: bold;
        text-align: center;
      }
      
      /* Style numeric input boxes */
      .form-control {
        background-color: #282828;
        color: white;
        border: 1px solid #535353;
        border-radius: 8px;
        font-size: 16px;
        padding: 10px;
        width: 100%;
        height: 45px;
        box-sizing: border-box;
      }
      
      /* Style input boxes when selected */
      .form-control:focus {
        background-color: #282828;
        color: white;
        border: 1px solid #1DB954;
        box-shadow: 0 0 5px #1DB954;
      }
      
      /* Style input labels */
      .control-label {
        color: white;
        font-weight: bold;
        font-size: 16px;
      }
      
      .pace-control {
        display: flex;
        align-items: center;
        width: 100%;
        gap: 4px;
        margin-bottom: 20px;
      }
      
      .pace-input {
        flex: 1;
        min-width: 0;
      }
      
      .pace-input .form-group {
        margin-bottom: 0;
      }
      
      .pace-input .form-control {
        background-color: #282828;
        color: white;
        border: 1px solid #535353;
        border-radius: 8px;
        font-size: 20px;
        text-align: center;
        height: 45px;
      }
      
      .pace-colon {
        color: white;
        font-size: 25px;
        font-weight: bold;
        margin: 0 2px;
      }
  "))
  ),
  
  # Title
  titlePanel("Running Song Generator"),
  
  # Text blurb + pic
  div(
    style = "
    display: flex;
    align-items: center;
    gap: 20px;
    margin: 20px 0;
  ",
    
    img(
      src = "Running.jpg",
      style = "
      width: 200px;
      height: 100px;
      object-fit: contain;
      flex-shrink: 0;
    "
    ),
    
    p(
      HTML(
        "This application uses a statistical model to predict your steps per minute (SPM) while running, 
        based on height, age, and desired pace.<sup>1</sup> Please input your running 
        information to generate songs with a beats per minute (BPM) close to your SPM!"
      ),
      style = "
      color: white;
      font-size: 20px;
      line-height: 1.5;
      margin: 0;
    "
    )
  ),
  
  # Green Divider
  div(
    style = "
    border-top: 2px solid #1DB954;
    margin: 25px 0;
  "
  ),
  
  # Title for Running Information
  h3("Input your Running Information", style = "
    color: white;
    font-size: 28px;
    font-weight: bold;
    text-align: center;
    margin-top: 20px;
    margin-bottom: 15px;"),
  
  # Height Input
  fluidRow(
    column(6, numericInput("height",
                           "Height (inches)",
                           value = 70,
                           min = 48,
                           max = 84,
                           width = "100%")
    ),
    # Age input
    column(6, numericInput("age",
                           "Age (years)",
                           value = 20,
                           min = 10,
                           max = 100,
                           width = "100%")
    )
  ),
  
  # Minutes and Seconds inputs
  # Pace Input
  tags$label("Intended Pace (minutes : seconds)", class = "control-label"),
  div(
    class = "pace-control",
    # Minutes box
    div(
      class = "pace-input",
      numericInput(
        "MINUTES",
        label = NULL,
        value = 8,
        min = 5,
        max = 15,
        step = 1,
        width = "100%"
      )
    ),
    # Colon box
    div(
      class = "pace-colon",
      ":"
    ),
    # Seconds box
    div(
      class = "pace-input",
      numericInput(
        "SECONDS",
        label = NULL,
        value = 0,
        min = 0,
        max = 59,
        step = 15,
        width = "100%"
      )
    )
  ),
  
  # Breaks
  br(),
  
  # Button to generate results
  actionButton("generate", "Generate Running Songs", class = "btn-custom", width = "100%"),
  
  # Breaks
  br(),
  br(),

  # Display estimated SPM
  uiOutput("estimated_spm"),
  
  # Title for suggested songs
  uiOutput("song_generator_title"),
  
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
  output$estimated_spm <- renderUI({
    
    req(estimated_spm())
    
    div(
      style = "
      text-align: center;
      background-color: #282828;
      padding: 10px;
      margin: 5px;
    ",
      
      h4(
        "YOUR ESTIMATED CADENCE",
        style = "color: white;"
      ),
      
      h1(
        paste(round(estimated_spm()), "Steps per Minute"),
        style = "
        color: #1DB954;
        font-size: 30px;
        font-weight: bold;
      "
      ),
    )
  })
  
  
  # Title for songs
  output$song_generator_title <- renderUI({
    req(estimated_spm())
    h3("Suggested Running Songs", style = "
    color: white;
    font-size: 28px;
    font-weight: bold;
    text-align: center;
    margin-top: 20px;
    margin-bottom: 15px;
  "
    )
  })
  
  # Filtering for songs that match the BPM
  matching_songs <- eventReactive(input$generate, {

    songs |>
      dplyr::filter(Tempo <= estimated_spm() + 5, Tempo >= estimated_spm() - 5)
  })
  
  # Create Spotify player
  output$spotify_player <- renderUI({
    
    req(input$selected_song)
    
    # Convert Spotify URI:
    # spotify:track:6epn3r7S14KUqlReYr77hA into: 6epn3r7S14KUqlReYr77hA
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
    
    # Selecting a subset of columns
    results |>
      # Rounding tempo
      dplyr::mutate(Tempo = round(Tempo)) |> 
      dplyr::select(Artist = Artist.Name.s., Song = Track.Name, Album = Album.Name, `Tempo (BPM)` = Tempo, Play)
    
  },
  escape = FALSE,
  selection = "none",
  options = list(
    pageLength = 10
  ))

}


# Run the application 
shinyApp(ui = ui, server = server)
