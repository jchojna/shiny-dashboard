library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)

# VARIABLES ----Znamirowice, 33-318
dataset <- read_csv("datasets/dataset.csv")
labels <- read_csv("datasets/labels.csv")

dataset <- dataset %>%
  separate(date, c("year", "month", "day"), "-", remove = FALSE, convert = TRUE)

years <- as.vector(unlist(distinct(dataset, year)))
lastDate <- tail(dataset, 1)$date
yesterday <- as.character(as.Date(lastDate) %m-% days(1))
dayBeforeYesterday <- as.character(as.Date(lastDate) %m-% days(2))

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

getStatsValue <- function(period, field) {
  endDate <- lastDate
  
  if (period == "Today") {
    startDate <- yesterday
    
  } else if (period == "Yesterday") {
    startDate <- dayBeforeYesterday
    endDate <- yesterday
    
  } else if (period == "Last Week") {
    startDate <- as.character(as.Date(lastDate) %m-% weeks(1))
    
  } else if (period == "Last Month") {
    startDate <- as.character(as.Date(lastDate) %m-% months(1))
    
  } else if (period == "Last Year") {
    startDate <- as.character(as.Date(lastDate) %m-% years(1))
  }
  
  currentTotal <- dataset %>%
    filter(date > startDate & date <= endDate) %>%
    select(field) %>%
    sum()
}

getPercentValue <- function(period, field) {
  
  if (period == "Today") {
    prevStartDate <- dayBeforeYesterday
    prevEndDate <- yesterday
    
  } else if (period == "Yesterday") {
    prevStartDate <- as.character(as.Date(lastDate) %m-% days(3))
    prevEndDate <- dayBeforeYesterday
    
  } else if (period == "Last Week") {
    prevStartDate <- as.character(as.Date(lastDate) %m-% weeks(2))
    prevEndDate <- as.character(as.Date(lastDate) %m-% weeks(1))
    
  } else if (period == "Last Month") {
    prevStartDate <- as.character(as.Date(lastDate) %m-% months(2))
    prevEndDate <- as.character(as.Date(lastDate) %m-% months(1))
    
  } else if (period == "Last Year") {
    prevStartDate <- as.character(as.Date(lastDate) %m-% years(2))
    prevEndDate <- as.character(as.Date(lastDate) %m-% years(1))
  }
  
  currentTotal <- getStatsValue(period, field)
  previousTotal <- dataset %>%
    filter(date > prevStartDate & date <= prevEndDate) %>%
    select(field) %>%
    sum()
  
  percentage <- (currentTotal - previousTotal) / previousTotal
  score <- abs(round(percentage, digits = 1))
  
  if (score == 0) {
    "stable"
  } else {
    paste(ifelse(percentage > 0, "+", "-"), score, sep="")
  }
}

# ----
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
        selectInput("period", "", na.omit(labels$period_name))
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
      p(class = "textPanel__value", textOutput("income", inline = TRUE)),
      div(
        class = "textPanel__percentage",
        # icon
        textOutput("incomePercent", inline = TRUE)
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
      p(class = "textPanel__value", textOutput("users", inline = TRUE)),
      div(
        class = "textPanel__percentage",
        # icon
        textOutput("usersPercent", inline = TRUE)
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
      p(class = "textPanel__value", textOutput("orders", inline = TRUE)),
      div(
        class = "textPanel__percentage",
        # icon
        textOutput("ordersPercent", inline = TRUE)
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
      p(class = "textPanel__value", textOutput("complaints", inline = TRUE)),
      div(
        class = "textPanel__percentage",
        # icon
        textOutput("complaintsPercent", inline = TRUE)
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
        selectInput("field", "", c(
          "Total Income"    = labels$field[1],
          "Active Users"    = labels$field[2],
          "New Orders"      = labels$field[3],
          "Open Complaints" = labels$field[4]
        ))
      ),
      div(
        class = "dropdown dropdown--month",
        selectInput("month", "", c(
          "All Months" = 0,
          "January"    = 1,
          "February"   = 2,
          "March"      = 3,
          "April"      = 4,
          "May"        = 5,
          "June"       = 6,
          "July"       = 7,
          "August"     = 8,
          "September"  = 9,
          "October"    = 10,
          "November"   = 11,
          "December"   = 12
        ))
      ),
      div(
        class = "dropdown dropdown--year",
        selectInput("year", "", years)
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
        class = "barChart",
        plotOutput("histogram", width = "100%", height = "100%")
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










# ----
# SERVER ----

server <- function(input, output) {
  
  # STATS OUTPUTS ----
  output$income <- renderText({
    period <- getStatsValue(input$period, "income")
  })
  output$users <- renderText({
    period <- getStatsValue(input$period, "users")
  })
  output$orders <- renderText({
    period <- getStatsValue(input$period, "orders")
  })
  output$complaints <- renderText({
    period <- getStatsValue(input$period, "complaints")
  })
  output$incomePercent <- renderText({
    period <- getPercentValue(input$period, "income")
  })
  output$usersPercent <- renderText({
    period <- getPercentValue(input$period, "users")
  })
  output$ordersPercent <- renderText({
    period <- getPercentValue(input$period, "orders")
  })
  output$complaintsPercent <- renderText({
    period <- getPercentValue(input$period, "complaints")
  })
  
  # HISTOGRAM OUTPUT ----
  output$histogram <- renderPlot({
    
    filtered_dt <- dataset %>%
      filter(year == input$year)
    
    if (input$month != 0) {
      filtered_dt <- filtered_dt %>%
        filter(month == input$month) %>%
        group_by(day)
  
    } else {
      filtered_dt <- filtered_dt %>%
        group_by(month)
    }
    
    filtered_dt <- filtered_dt %>%
      summarize(sum = sum(get(input$field)))
      
    ggplot(filtered_dt, aes(x=day, y=sum, color="blue")) +
      geom_col()
      
      
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
