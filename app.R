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
    HTML("
      <svg class = 'app__logo' viewBox = '0 0 100 100'>
        <use href = 'svg/icons.svg#logo'></use>
      </svg>
    ")
  ),
  
  # STATS SECTION ----
  tags$section(
    class = "app__section app__section--stats",
    # STATS HEADER ----
    tags$header(
      class = "app__header app__header--stats",
      h2(class = "app__heading app__heading--section", "Recent"),
      div(
        class = "dropdown dropdown--period",
        selectInput("period", "",
          list("Today", "Yesterday", "Last Week", "Last Month", "Last Year")
        )
      )
    ),
    # STATS INCOME ----
    tags$section(
      class="textPanel textPanel--income",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Total Income"),
        HTML("
          <svg class = 'app__logo' viewBox = '0 0 100 100'>
            <use href = 'svg/icons.svg#income'></use>
          </svg>
        ")
      )
    ),
    # STATS USERS ----
    tags$section(
      class="textPanel textPanel--users",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Active Users"),
        HTML("
          <svg class = 'app__logo' viewBox = '0 0 100 100'>
            <use href = 'svg/icons.svg#users'></use>
          </svg>
        ")
      )
    ),
    # STATS ORDERS ----
    tags$section(
      class="textPanel textPanel--orders",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "New Orders"),
        HTML("
          <svg class = 'app__logo' viewBox = '0 0 100 100'>
            <use href = 'svg/icons.svg#orders'></use>
          </svg>
        ")
      )
    ),
    # STATS COMPLAINTS ----
    tags$section(
      class="textPanel textPanel--complaints",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Open Complaints"),
        HTML("
          <svg class = 'app__logo' viewBox = '0 0 100 100'>
            <use href = 'svg/icons.svg#complaints'></use>
          </svg>
        ")
      )
    ),
  ),
  
  
  
  
  
  
  
  
  
  
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
