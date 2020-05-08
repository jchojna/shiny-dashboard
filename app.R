library(shiny)

ui <- fluidPage(
  theme = "styles/main.css",
  class = "app",
  
  # HEAD TAG ----
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(
      name = "shinyDashboard",
      content = "Dashboard app created in R Shiny"
    ),
    tags$meta(property="og:title", content="shinyDashboard"),
    tags$meta(
      property="og:description",
      content="Dashboard app created in R Shiny"
    ),
    tags$meta(property="og:type", content="website"),
    tags$meta(property="og:url", content="https://jchojna.github.io/shiny-dashboard/"),
    tags$meta(
      property="og:image",
      content="https://jchojna.github.io/shiny-dashboard/icons/icon-512x512.png"
    ),
    tags$meta(property="og:image:width", content="512"),
    tags$meta(property="og:image:height", content="512"),
    tags$meta(name="apple-mobile-web-app-capable", content="yes"),
    
    tags$link(
      rel = "apple-touch-icon", sizes = "192x192", href = "icons/icon-192x192.png"
    ),
    tags$link(rel = "icon", type = "image/png", href = "icons/icon-192x192.png"),
    tags$link(
      href="https://fonts.googleapis.com/css2?family=Nunito:wght@400;700&display=swap",
      rel="stylesheet"
    ),
    tags$title("shinyDashboard")
  ),
  
  # PAGE HEADER ----
  tags$header(
    class = "app__header app__header--main",
    h1(class = "app__heading", "shinyDashboard"),
    HTML(
      "
      <svg class = 'app__logo' viewBox = '0 0 100 100'>
        <use href = 'svg/icons.svg#logo'></use>
      </svg>
    "
    )
  ),
  
  tags$section(
    class = "app__section app__section--stats",
    
    tags$header(
      class = "app__header app__header--stats",
      h2(class = "app__heading app__heading--section", "Recent"),
      div(
        class = "dropdown dropdown--period",
        selectInput("period", "",
          list("Today", "Yesterday", "Last Week", "Last Month", "Last Year")
        )
      )
    )
  ),
  
  
  
  #   <Dropdown
  # currentId={period}
  # type="period"
  # isDataLoading={isDataLoading}
  # menuList={statsPeriods}
  # onMenuClick={this.handleStats}
  # />
  
  
  
  
  
  
  
  
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = '#AC2476',
         border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
