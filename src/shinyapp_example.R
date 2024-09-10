library(shiny)

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
      tableOutput("filteredTable")
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
    if (input$product_id != "") {
      data <- subset(data, Product_ID == input$product_id)
    }
    if (input$product_name != "") {
      data <- subset(data, Product_Name == input$product_name)
    }
    if (input$keywords != "") {
      data <- subset(data, Keywords == input$keywords)
    }
    if (input$location != "All") {
      data <- subset(data, Location == input$location)
    }
    if (input$steward != "All") {
      data <- subset(data, Steward == input$steward)
    }
    if (input$users != "All") {
      data <- subset(data, Users == input$users)
    }
    if (input$pii != "All") {
      data <- subset(data, PII == input$pii)
    }
    if (input$source != "All") {
      data <- subset(data, Source == input$source)
    }
    
    # Select only the Connection column for display
    data <- data[, c("Connection")]
    
    return(data)
  })
  
  # Render the filtered table
  output$filteredTable <- renderTable({
    filteredData()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
