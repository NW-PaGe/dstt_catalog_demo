library(shiny)
library(DiagrammeR)
library(dplyr)
library(tidyr)

# Read the CSV file when the script starts
metadata <- read.csv(url("https://raw.githubusercontent.com/NW-PaGe/dstt_catalog_demo/refs/heads/main/metadata.csv"))

# Define UI for the app
ui <- fluidPage(
  titlePanel("Directed Acyclic Graph (DAG):"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select nodes"),
      checkboxGroupInput("selected_nodes", "Nodes", choices = unique(metadata$Product_Name), selected = unique(metadata$Product_Name)),
      hr()
    ),
    
    mainPanel(
      grVizOutput("dag_plot", width = "100%", height = "600px")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Create a reactive DAG from selected nodes
  reactive_dag <- reactive({
    ### Create nodes based on inclusion in selected input
    nodes_final <- metadata %>% 
      filter(Product_Name %in% input$selected_nodes) %>%
      select(Product_Name) %>% 
      mutate(Product_Name_Copy = Product_Name) %>% 
      distinct()
    
    ### Create edges based on `Parent` and `Child` columns ----------------
    
    # unnest any nodes with 2+ parents or children
    unnested_data <- metadata %>% 
      select(Product_Name, Parent, Child) %>% 
      mutate(Product_Name_Copy = Product_Name) %>% 
      tidyr::separate_rows(Parent, sep = ";") %>% 
      tidyr::separate_rows(Child, sep = ";") %>% 
      distinct()
    
    # identify all possible parents
    parent_df <- unnested_data %>% 
      filter(!is.na(Parent)) %>% 
      mutate(Child = Product_Name)

    # identify all possible children
    child_df <- unnested_data %>% 
      filter(!is.na(Child)) %>% 
      mutate(Parent = Product_Name)
    
    # join parents and children to get all possible edges
    edges <- rbind(parent_df, child_df) %>% 
      select(Parent, Child) %>% 
      distinct()
    
    # get direct edges (edges where no product has been deselected)
    direct_edges <- edges %>% 
      filter(Parent %in% input$selected_nodes & Child %in% input$selected_nodes) %>% 
      mutate(style = 'solid') # set edge attribute so line will be solid
    
    # get indirect edges (edges where child or parent has been deselected)
    indirect_parent <- edges %>% 
      filter(!Parent %in% input$selected_nodes) %>% 
      rename(removed=Parent)
    indirect_child <- edges %>% 
      filter(!Child %in% input$selected_nodes) %>% 
      rename(removed=Child)
    indirect_edges <- indirect_parent %>% 
      left_join(indirect_child, by='removed') %>% 
      select(-removed) %>% 
      mutate(style = 'dashed') # set edge attribute so line will be dashed
    
    # concat dfs for final edges df with direct/indirect relationships included
    edges_final <- rbind(indirect_edges, direct_edges)
    
    ### Create the DAG using DiagrammeR ------------------------------------------
    dag <- create_graph() %>%
      add_nodes_from_table(table = nodes_final,
                           label_col = "Product_Name") %>%
      add_edges_from_table(table = edges_final,
                           from_col = "Parent",
                           to_col = "Child",
                           from_to_map = "Product_Name_Copy")

    return(dag)
  })
  
  # Render the DAG plot
  output$dag_plot <- renderGrViz({
    dag <- reactive_dag()
    dag %>% render_graph()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
