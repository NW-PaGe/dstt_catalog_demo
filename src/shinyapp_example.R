library(shiny)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Filtered Metadata Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Static text inputs for certain columns
      textInput("product_id", "Product ID", ""),
      textInput("product_name", "Product Name", ""),
      textInput("keywords", "Keywords", ""),
      
      # Dynamic inputs for filtering based on unique values
      uiOutput("locationSelect"),
      uiOutput("stewardSelect"),
      uiOutput("usersSelect"),
      uiOutput("piiSelect"),
      uiOutput("sourceSelect")
    ),
    mainPanel(
      h3("Filters Applied:"),
      verbatimTextOutput("filtersSummary"),
      br(),
      h3("Filtered File Paths:"),
      uiOutput("fileList"),  # To display the list of file paths
      br(),
      h3("Selected File Data:"),
      DTOutput("fileDataTable")  # To display data from the selected file
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Read the CSV file when the app starts
  metadata <- read.csv("metadata.csv")
  
  # Dynamically create selectInputs for each filterable column
  output$locationSelect <- renderUI({
    unique_locations <- unique(metadata$Location)
    location_choices <- c("All", unique_locations)
    selectInput("location", "Location", choices = location_choices, selected = "All")
  })

  output$stewardSelect <- renderUI({
    unique_stewards <- unique(metadata$Steward)
    steward_choices <- c("All", unique_stewards)
    selectInput("steward", "Steward", choices = steward_choices, selected = "All")
  })

  output$usersSelect <- renderUI({
    unique_users <- unique(metadata$Users)
    users_choices <- c("All", unique_users)
    selectInput("users", "Users", choices = users_choices, selected = "All")
  })

  output$piiSelect <- renderUI({
    unique_pii <- unique(metadata$PII)
    pii_choices <- c("All", unique_pii)
    selectInput("pii", "PII", choices = pii_choices, selected = "All")
  })

  output$sourceSelect <- renderUI({
    unique_sources <- unique(metadata$Source)
    source_choices <- c("All", unique_sources)
    selectInput("source", "Source", choices = source_choices, selected = "All")
  })
  
  # Reactive expression to filter data based on inputs
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

  output$fileList <- renderUI({
    file_paths <- filteredData()$Connection  # Assuming "Connection" column contains file paths
    if (length(file_paths) == 0) {
      return("No files match the current filters.")
    }
    # Create a clickable link for each file path, each on its own line
    tags$ul(
      lapply(seq_along(file_paths), function(i) {
        path <- file_paths[i]
        tags$li(
          actionLink(inputId = paste0("file_", i), label = path)
        )
      })
    )
  })
  
  # Reactive expression to create a summary of applied filters
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
      file_data <- read.csv(selectedFile())
      datatable(file_data, options = list(pageLength = 10, autoWidth = TRUE))
    }, error = function(e) {
      # If there's an error reading the file, display an error message
      datatable(data.frame(Error = paste("Unable to read file:", e$message)),
                options = list(pageLength = 1, dom = 't'))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
