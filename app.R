library(shiny)

ui <- fluidPage(theme = "styles/main.css", class = "app",

  # PAGE HEADER
  tags$header(
    class = "app__header",
    h1(class = "app__heading", "ShinyDashboard"),
    img(class = "app__logo", src = "svg/logo.svg")
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = '#AC2476', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
