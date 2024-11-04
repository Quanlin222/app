library(shiny)
library(devtools)

# Install and load the Five package before starting the application
if (!require(api)) {
  devtools::install_github("Quanlin222/api")
}

# Load the app package
library(api)

ui <- fluidPage(
  titlePanel("Interactive Data Analysis"),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "Year:",
                  choices = 2000:2023,
                  selected = 2020),

      selectInput(inputId = "filterValue",
                  label = "Filter values greater than:",
                  choices = c(100, 130, 140),
                  selected = 100)
    ),
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output) {

  dataset <- reactive({
    year <- as.numeric(input$dataset)
    data <- getdata_api(year)

    # 确保 data 不为 NULL
    if (is.null(data)) {
      return(data.frame())  # 返回空数据框以避免后续错误
    }

    data_frame <- data$values

    # Removing commas and other non-numeric characters
    data_frame$values <- gsub("[^0-9.]", "", data_frame$values)

    # Convert to numeric
    data_frame$values <- suppressWarnings(as.numeric(data_frame$values))

    return(data_frame)
  })

  filteredData <- reactive({
    data <- dataset()
    filterValue <- as.numeric(input$filterValue)

    # 确保 data 不为空
    if (nrow(data) == 0) {
      return(data.frame())  # 返回空数据框以避免后续错误
    }

    filtered <- data[data$values > filterValue, c("kpi", "municipality", "period", "values"), drop = FALSE]
    return(filtered)
  })

  output$dataTable <- renderTable({
    filteredData()
  })
}

shinyApp(ui = ui, server = server)
