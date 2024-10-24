library(shiny)
library(DT)
library(officer)

# Read the CSV file when the script starts
metadata <- read.csv(url("https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/refs/heads/main/metadata.csv"))

# Define the UI
ui <- fluidPage(
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .filter-tag {
        display: inline-block;
        margin: 3px;
        padding: 6px 12px;
        background-color: #3498db;
        color: white;
        border-radius: 15px;
        cursor: pointer;
        font-size: 14px;
        transition: background-color 0.3s ease;
      }
      .filter-tag:hover {
        background-color: #2980b9;
      }
      .filter-label {
        font-weight: bold;
        margin-right: 5px;
      }
      .filter-value {
        font-weight: normal;
      }
      .remove-icon {
        margin-left: 8px;
        font-weight: bold;
        opacity: 0.8;
      }
      .filter-tag:hover .remove-icon {
        opacity: 1;
      }
    "))
  ),
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
      uiOutput("filtersSummary"),
      br(),
      h3("Filtered File Paths:"),
      uiOutput("fileList"),
      br(),
      h3("Selected File Data:"),
      DTOutput("fileDataTable"),
      uiOutput("docx_content"),
      br()
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observe({
    output$fileList <- renderUI({
      file_paths <- filteredData()$Connection
      tags$ul(
        lapply(seq_along(file_paths), function(i) {
          tags$li(
            actionLink(
              inputId = paste0("file_", i),
              label = file_paths[i],
              onclick = sprintf("Shiny.setInputValue('last_clicked', '%s', {priority: 'event'});", paste0("file_", i))
            )
          )
        })
      )
    })
  }, priority = 1000)
  
  # Original filtering logic remains the same
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
  
  # Modified filters summary with enhanced styling
  output$filtersSummary <- renderUI({
    filters <- list()
    
    if (!is.null(input$product_id) && input$product_id != "") {
      filters$product_id <- list(label = "Product ID", value = input$product_id)
    }
    if (!is.null(input$product_name) && input$product_name != "") {
      filters$product_name <- list(label = "Product Name", value = input$product_name)
    }
    if (!is.null(input$keywords) && input$keywords != "") {
      filters$keywords <- list(label = "Keywords", value = input$keywords)
    }
    if (!is.null(input$location) && input$location != "All") {
      filters$location <- list(label = "Location", value = input$location)
    }
    if (!is.null(input$steward) && input$steward != "All") {
      filters$steward <- list(label = "Steward", value = input$steward)
    }
    if (!is.null(input$users) && input$users != "All") {
      filters$users <- list(label = "Users", value = input$users)
    }
    if (!is.null(input$pii) && input$pii != "All") {
      filters$pii <- list(label = "PII", value = input$pii)
    }
    if (!is.null(input$source) && input$source != "All") {
      filters$source <- list(label = "Source", value = input$source)
    }
    
    if (length(filters) == 0) {
      return(p("No filters applied."))
    } else {
      # Create interactive filter tags with enhanced styling
      tags$div(
        lapply(names(filters), function(filter_name) {
          tags$div(
            class = "filter-tag",
            onclick = sprintf("Shiny.setInputValue('remove_filter', '%s', {priority: 'event'});", filter_name),
            tags$span(
              class = "filter-label",
              filters[[filter_name]]$label
            ),
            tags$span(
              class = "filter-value",
              filters[[filter_name]]$value
            ),
            tags$span(
              class = "remove-icon",
              HTML("&times;")
            )
          )
        })
      )
    }
  })
  
  # Observer to handle filter removal
  observeEvent(input$remove_filter, {
    filter_name <- input$remove_filter
    if (filter_name %in% c("product_id", "product_name", "keywords")) {
      updateTextInput(session, filter_name, value = "")
    } else {
      updateSelectInput(session, filter_name, selected = "All")
    }
  })
  
  # Reactive value to store the selected file path
  selectedFile <- reactiveVal(NULL)
  
  # Replace the original observer with this new one that responds to last_clicked
  observeEvent(input$last_clicked, {
    file_index <- as.numeric(gsub("file_", "", input$last_clicked))
    file_paths <- filteredData()$Connection
    if (file_index <= length(file_paths)) {
      selectedFile(file_paths[file_index])
    }
  }, ignoreInit = TRUE)
  
  # Render the data from the selected file
  output$fileDataTable <- renderDT({
    req(selectedFile())  # Ensure there is a selected file
    if (grepl('.csv', selectedFile())) {
      tryCatch({
        file_data <- read.csv(url(paste0('https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/refs/heads/main/', selectedFile())))
        datatable(file_data, options = list(pageLength = 10, autoWidth = TRUE))
      }, error = function(e) {
        datatable(data.frame(Error = paste("Unable to read file:", e$message)),
                  options = list(pageLength = 1, dom = 't'))
      })
    } else {
      datatable(data.frame())
    }
  })
  
  output$docx_content <- renderUI({
    req(selectedFile())  # Ensure there is a selected file
    if (grepl('.docx', selectedFile())) {
      tryCatch({
        url <- paste0('https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/main/', selectedFile())
        temp_file <- tempfile(fileext = ".docx")
        download.file(url, destfile = temp_file, mode = "wb")
        # Read the .docx file using officer
        doc <- read_docx(temp_file)
        # Convert docx to html element using code from src
        docx_to_html(doc)
      }, error = function(e) {
        HTML(paste("Unable to read file:", e$message))
      })
    } else {
      HTML('')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
