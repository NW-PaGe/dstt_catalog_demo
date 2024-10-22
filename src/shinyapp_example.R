library(shiny)
library(DT)

# Read the CSV file when the script starts
metadata <- read.csv(url("https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/refs/heads/main/metadata.csv"))

# Define the UI
ui <- fluidPage(
  titlePanel("Filtered Metadata Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Static text inputs for certain columns
      textInput("product_id", "Product ID", ""),
      textInput("product_name", "Product Name", ""),
      textInput("keywords", "Keywords", ""),
      
      # Static text inputs dynamically pulled from metadata.csv for certain columns
      selectInput("location", "Location", choices = c("All", unique(metadata$Location)), selected = "All"),
      selectInput("steward", "Steward", choices = c("All", unique(metadata$Steward)), selected = "All"),
      selectInput("users", "Users", choices = c("All", unique(metadata$Users)), selected = "All"),
      selectInput("pii", "PII", choices = c("All", unique(metadata$PII)), selected = "All"),
      selectInput("source", "Source", choices = c("All", unique(metadata$Source)), selected = "All")
    ),
    mainPanel(
      h3("Filters Applied:"),
      verbatimTextOutput("filtersSummary"),
      br(),
      h3("Filtered File Paths:"),
      uiOutput("fileList"),
      br(),
      h3("Selected File Data:"),
      DTOutput("fileDataTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # The only addition needed for the fix
  observe({
    output$fileList <- renderUI({
      file_paths <- filteredData()$Connection
      tags$ul(
        lapply(seq_along(file_paths), function(i) {
          tags$li(
            actionLink(inputId = paste0("file_", i), label = file_paths[i])
          )
        })
      )
    })
  }, priority = 1000)
  
  # Original code continues as normal...
  filteredData <- reactive({
    data <- metadata
    
    # Apply filters based on input values
    if (!is.null(input$product_id) && input$product_id != "") {
      data <- subset(data, Product_ID == input$product_id)
    }
    if (!is.null(input$product_name) && input$product_name != "") {
      data <- subset(data, Product_Name == input$product_name)
    }
    if (!is.null(input$keywords) && input$keywords != "") {
      data <- subset(data, Keywords == input$keywords)
    }
    if (!is.null(input$location) && input$location != "All") {
      data <- subset(data, Location == input$location)
    }
    if (!is.null(input$steward) && input$steward != "All") {
      data <- subset(data, Steward == input$steward)
    }
    if (!is.null(input$users) && input$users != "All") {
      data <- subset(data, Users == input$users)
    }
    if (!is.null(input$pii) && input$pii != "All") {
      data <- subset(data, PII == input$pii)
    }
    if (!is.null(input$source) && input$source != "All") {
      data <- subset(data, Source == input$source)
    }
    
    return(data)
  })
  
  output$filtersSummary <- renderText({
    filters <- c()
    
    if (!is.null(input$product_id) && input$product_id != "") {
      filters <- c(filters, paste("Product ID:", input$product_id))
    }
    if (!is.null(input$product_name) && input$product_name != "") {
      filters <- c(filters, paste("Product Name:", input$product_name))
    }
    if (!is.null(input$keywords) && input$keywords != "") {
      filters <- c(filters, paste("Keywords:", input$keywords))
    }
    if (!is.null(input$location) && input$location != "All") {
      filters <- c(filters, paste("Location:", input$location))
    }
    if (!is.null(input$steward) && input$steward != "All") {
      filters <- c(filters, paste("Steward:", input$steward))
    }
    if (!is.null(input$users) && input$users != "All") {
      filters <- c(filters, paste("Users:", input$users))
    }
    if (!is.null(input$pii) && input$pii != "All") {
      filters <- c(filters, paste("PII:", input$pii))
    }
    if (!is.null(input$source) && input$source != "All") {
      filters <- c(filters, paste("Source:", input$source))
    }
    
    if (length(filters) == 0) {
      return("No filters applied.")
    } else {
      return(paste(filters, collapse = "\n"))
    }
  })
  
  # Reactive value to store the selected file path
  selectedFile <- reactiveVal(NULL)
  
  # Observer to update the selected file path when a link is clicked
  observe({
    file_paths <- filteredData()$Connection
    for (i in seq_along(file_paths)) {
      if (isTruthy(input[[paste0("file_", i)]])) {
        selectedFile(file_paths[i])
        break
      }
    }
  })
  
  # Render the data from the selected file
  output$fileDataTable <- renderDT({
    req(selectedFile())  # Ensure there is a selected file
    
    tryCatch({
      file_data <- read.csv(url(paste0('https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/refs/heads/main/', selectedFile())))
      datatable(file_data, options = list(pageLength = 10, autoWidth = TRUE))
    }, error = function(e) {
      datatable(data.frame(Error = paste("Unable to read file:", e$message)),
                options = list(pageLength = 1, dom = 't'))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
