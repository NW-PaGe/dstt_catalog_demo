library(shiny)
library(DiagrammeR)
library(dplyr)
library(tidyr)

# Sample data frame with node and edge information
metadata <- structure(
  list(
    Product_ID = c(4047648790, 5754199350, 9000442265, 
                   9325481063, 6072493491, 5645041168, 3344276255, 2307679692, 1512337206, 
                   2824302477, 3748996607, 2008766938, 7300648833, 3855770051, 3092745195, 
                   4520627002, 3932969825, 6873647526, 8109343197, 9323872357, 7334703566, 
                   8858668888, 7829333381, 9936639753, 2463472188, 8971627055, 7253259313, 
                   9715465027, 8085881829, 7394149033, 3590275606), 
    Product_Name = c("wa_genbank", 
                     "wa_genbank", "wa_genbank", "wa_genbank", "lineages", "lineages", 
                     "lineages", "lineages", "lineages", "lineages", "lineages", "lineages", 
                     "lineages", "lineages", "lineages", "lineages", "lineages", "lineages", 
                     "lineages", "lineages", "lineages", "lineages", "seq_results", 
                     "seq_results", "seq_results", "seq_results", "seq_report", "genbank", 
                     "genbank", "genbank", "genbank"), 
    Location = c("network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive", 
                 "network_drive", "local", "local", "local", "local", "network_drive", 
                 "network_drive", "network_drive", "network_drive", "network_drive"), 
    Steward = c("DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", 
                "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", 
                "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", "DIQA", 
                "PHL", "PHL", "PHL", "PHL", "MEP", "DIQA", "DIQA", "DIQA", "DIQA"), 
    Source = c("genbank", "genbank", "genbank", "genbank", "CDC", 
               "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", 
               "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "CDC", "BINFX", 
               "BINFX", "BINFX", "BINFX", "", "NCBI", "NCBI", "NCBI", "NCBI"), 
    Users = c("DIQA; MEP", "DIQA; MEP", "DIQA; MEP", "DIQA; MEP", 
              "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", 
              "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", "MEP", 
              "PHL", "PHL", "PHL", "PHL", "Public", "DIQA", "DIQA", "DIQA", 
              "DIQA"), 
    Keywords = c("ncbi;results;accessions;virus;refseq;upload", 
                 "ncbi;results;accessions;virus;refseq;upload", "ncbi;results;accessions;virus;refseq;upload", 
                 "ncbi;results;accessions;virus;refseq;upload", "variant", "variant", 
                 "variant", "variant", "variant", "variant", "variant", "variant", 
                 "variant", "variant", "variant", "variant", "variant", "variant", 
                 "variant", "variant", "variant", "variant", "sequencing;results;accessions;variants;lineages", 
                 "sequencing;results;accessions;variants;lineages", "sequencing;results;accessions;variants;lineages", 
                 "sequencing;results;accessions;variants;lineages", "report:variants;lineages", 
                 "ncbi;genbank;lineages;repository;refseq", "ncbi;genbank;lineages;repository;refseq", 
                 "ncbi;genbank;lineages;repository;refseq", "ncbi;genbank;lineages;repository;refseq"), 
    PII = c("No", "No", "No", "No", "No", "No", "No", "No", "No", 
            "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", 
            "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "Yes"), 
    Connection = c("data/wa_genbank/wa_genbank_2024-10-21.csv", 
                   "data/wa_genbank/wa_genbank_2024-10-14.csv", "data/wa_genbank/wa_genbank_2024-10-07.csv", 
                   "data/wa_genbank/wa_genbank_2024-09-30.csv", "data/lineages/Lineages_2024-06-03.csv", 
                   "data/lineages/Lineages_2024-06-04.csv", "data/lineages/Lineages_2024-06-05.csv", 
                   "data/lineages/Lineages_2024-06-06.csv", "data/lineages/Lineages_2024-06-07.csv", 
                   "data/lineages/Lineages_2024-06-10.csv", "data/lineages/Lineages_2024-06-11.csv", 
                   "data/lineages/Lineages_2024-06-12.csv", "data/lineages/Lineages_2024-06-13.csv", 
                   "data/lineages/Lineages_2024-06-14.csv", "data/lineages/Lineages_2024-06-17.csv", 
                   "data/lineages/Lineages_2024-06-18.csv", "data/lineages/Lineages_2024-06-19.csv", 
                   "data/lineages/Lineages_2024-06-20.csv", "data/lineages/Lineages_2024-06-21.csv", 
                   "data/lineages/Lineages_2024-06-24.csv", "data/lineages/Lineages_2024-06-25.csv", 
                   "data/lineages/Lineages_2024-06-26.csv", "data/seq_results/results_with_ncbi_run163.csv", 
                   "data/seq_results/results_with_ncbi_run170.csv", "data/seq_results/results_with_ncbi_run172.csv", 
                   "data/seq_results/results_with_ncbi_run172b.csv", "data/seq_report/SequencingReport_2024-01-30.docx", 
                   "data/genbank/raw_genbank_2024-09-30.csv", "data/genbank/raw_genbank_2024-10-07.csv", 
                   "data/genbank/raw_genbank_2024-10-14.csv", "data/genbank/raw_genbank_2024-10-21.csv"), 
    parent = c("genbank", "genbank", "genbank", "genbank", NA, 
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
               NA, NA, NA, NA, NA, "seq_results;lineages;wa_genbank", NA, NA, 
               NA, NA), 
    child = c("seq_report", "seq_report", "seq_report", 
              "seq_report", "seq_report", "seq_report", "seq_report", "seq_report", 
              "seq_report", "seq_report", "seq_report", "seq_report", "seq_report", 
              "seq_report", "seq_report", "seq_report", "seq_report", "seq_report", 
              "seq_report", "seq_report", "seq_report", "seq_report", "seq_report", 
              "seq_report", "seq_report", "seq_report", NA, "wa_genbank", "wa_genbank", 
              "wa_genbank", "wa_genbank")
  ), 
  class = "data.frame", row.names = c(NA, -31L)
)

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
    
    ### Create edges based on `parent` and `child` columns ----------------
    
    # unnest any nodes with 2+ parents or children
    unnested_data <- metadata %>% 
      select(Product_Name, parent, child) %>% 
      mutate(Product_Name_Copy = Product_Name) %>% 
      tidyr::separate_rows(parent, sep = ";") %>% 
      tidyr::separate_rows(child, sep = ";") %>% 
      distinct()
    
    # identify all possible parents
    parent <- unnested_data %>% 
      filter(!is.na(parent)) %>% 
      mutate(child = Product_Name)

    # identify all possible children
    child <- unnested_data %>% 
      filter(!is.na(child)) %>% 
      mutate(parent = Product_Name)
    
    # join parents and children to get all possible edges
    edges <- rbind(parent, child) %>% 
      select(parent, child) %>% 
      distinct()
    
    # get direct edges (edges where no product has been deselected)
    direct_edges <- edges %>% 
      filter(parent %in% input$selected_nodes & child %in% input$selected_nodes) %>% 
      mutate(style = 'solid') # set edge attribute so line will be solid
    
    # get indirect edges (edges where child or parent has been deselected)
    indirect_parent <- edges %>% 
      filter(!parent %in% input$selected_nodes) %>% 
      rename(removed=parent)
    indirect_child <- edges %>% 
      filter(!child %in% input$selected_nodes) %>% 
      rename(removed=child)
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
                           from_col = "parent",
                           to_col = "child",
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
