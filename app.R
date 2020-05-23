library(shiny)
library(dplyr)
library(readr)

dataset <- read_csv("datasets/dataset.csv")
labels <- read_csv("datasets/labels.csv")

totalComplaints <- sum(dataset$income)

# FUNCTIONS ----
svgIcon <- function(id) {
  return (
    HTML(
      paste(
        "<svg
          class = 'textPanel__icon textPanel__icon--", id, "'
          viewBox = '0 0 100 100'
        >
          <use href = 'svg/icons.svg#", id, "'></use>
        </svg>", sep=""
      )
    )
  )
}

# LAYOUT ----
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
        selectInput("period", "", na.omit(labels$period))
      )
    ),
    
    # STATS INCOME ----
    tags$section(
      class="textPanel textPanel--income",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Total Income"),
        svgIcon("income")
      ),
      p(class = "textPanel__value", 45),
      div(
        class = "textPanel__percentage",
        # icon
        span("+0.5%")
      )
    ),
    
    # STATS USERS ----
    tags$section(
      class="textPanel textPanel--users",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Active Users"),
        svgIcon("users")
      ),
      p(class = "textPanel__value", 45),
      div(
        class = "textPanel__percentage",
        # icon
        span("+0.5%")
      )
    ),
    
    # STATS ORDERS ----
    tags$section(
      class="textPanel textPanel--orders",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "New Orders"),
        svgIcon("orders")
      ),
      p(class = "textPanel__value", 45),
      div(
        class = "textPanel__percentage",
        # icon
        span("+0.5%")
      )
    ),
    # STATS COMPLAINTS ----
    tags$section(
      class="textPanel textPanel--complaints",
      tags$header(
        class = "textPanel__header",
        h3(class = "textPanel__heading", "Open Complaints"),
        svgIcon("complaints")
      ),
      p(class = "textPanel__value", 45),
      div(
        class = "textPanel__percentage",
        # icon
        span("+0.5%")
      )
    ),
  ),
  
  # ANALYTICS SECTION ----
  tags$section(
    class = "app__section app__section--analytics",
    
    # ANALYTICS HEADER ----
    tags$header(
      class = "app__header app__header--analytics",
      h2(class = "app__heading app__heading--section", "Analytics"),
      div(
        class = "dropdown dropdown--field",
        selectInput("field", "", na.omit(labels$field_label))
      ),
      div(
        class = "dropdown dropdown--month",
        selectInput("field", "", labels$month_name)
      ),
      div(
        class = "dropdown dropdown--year",
        selectInput("field", "",
          c(2020, 2019, 2018, 2017)
        )
      ),
    ),
    
    # HISTOGRAM SECTION ----
    tags$section(
      class = "visualPanel visualPanel--histogram",
      tags$header(
        class = "visualPanel__header",
        h3(class = "visualPanel__heading", "Total Income"),
        tags$button(
          class = "visualPanel__button",
          "MAX"
        )
      ),
      div(
        class = "barChart"
      )
    ),
    
    # MAP SECTION ----
    tags$section(
      class = "visualPanel visualPanel--map",
      tags$header(
        class = "visualPanel__header",
        h3(class = "visualPanel__heading", "Map"),
        tags$button(
          class = "visualPanel__button",
          "MAX"
        )
      ),
      div(
        class = "map"
      )
    ),
    
    # SUMMARY SECTION ----
    tags$section(
      class = "visualPanel visualPanel--summary",
      tags$header(
        class = "visualPanel__header",
        h3(class = "visualPanel__heading", "Summary"),
        tags$button(
          class = "visualPanel__button",
          "MAX"
        )
      ),
      div(
        class = "barChart"
      )
    ),
    
    # FOOTER ----
    tags$footer(
      class = "app__footer",
      tags$button(class = "button", tags$span(class = "button__text", "export")),
      tags$button(class = "button", tags$span(class = "button__text", "print"))
    )
  )
)










#############
# SERVER ----

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

# Run the application ----
shinyApp(ui = ui, server = server)
