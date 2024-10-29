### write_metadata.R
### This script can be sourced on your local machine to interactively write a new metadata row to metadata.csv
### Working directory should be set to head of cloned repo (dstt_catalog_demo/)

# Read in current metadata file
metadata <- read.csv('metadata.csv')

# import unid assigner
source('src/generate_unid.R')

# create function to ask for metadata inputs and validate input
get_value <- function(col, name=NA) {
  valid <- 'n'
  while (tolower(valid) != 'y') {
    cat('\n')
    # get set of example values to print
    example_vals <- unique(metadata[col])
    example_vals <- example_vals[!is.na(example_vals) & example_vals != '']
    example_vals <- example_vals[1:min(c(5, length(example_vals)))]
    example_vals <- paste(example_vals, collapse = ', ')
    if (nchar(example_vals) > 80) {
      example_vals <- paste0(substr(example_vals, 1, 80), '...')
    }
    prompted <- FALSE
    # get set of values specific to existing product name (if applicable)
    if (!is.na(name)) {
      prompted <- TRUE # set flag to not repeat prompt later
      product_vals <- unique(metadata[metadata$Product_Name==name, col])
      product_vals <- product_vals[!is.na(product_vals) & product_vals != '']
      if (length(product_vals) > 0) {
        # ask for column value
        cat(paste0('Enter ', col, ': (e.g., ', example_vals, ') \n'))
        cat(paste0(1:length(product_vals), ': ', product_vals, ' \n', collapse=""))
        product_prompt <- 'Enter # to duplicate existing value or enter new value: '
        col_value <- readline(product_prompt)
        if (col_value %in% paste0(1:length(product_vals))) {
          col_value <- product_vals[as.numeric(col_value)]
        }
      } else {
        prompted <- FALSE # don't prompt w/existing values if none exist
      }
    }
    # run this prompt if existing values were not used
    if (!prompted) {
      # ask for column value
      prompt <- paste0('Enter ', col, ': (e.g., ', example_vals, ') ')
      col_value <- readline(prompt)
    }
    
    # print typed value and confirm
    cat(col, ': ', col_value, '\n', sep='')
    cat('y: continue\n')
    cat('n: re-enter value\n')
    valid <- readline('Continue? y/n ')
  }
  
  return(col_value)
}

# Connection ----------------------------
# ask for data connection and validate
connection <- 'data'
while(file.info(connection)$isdir) {
  # get files & subdirectories of current connection path
  files = list.files(connection)
  # go back if no files are present in directory
  if (length(files) == 0) {
    cat('No files present. Going to previous dir.')
    connection <- connection_old
  } else {
    # Print the files with numbers
    cat("Select a file or folder by entering the corresponding number:\n")
    for (i in seq_along(files)) {
      cat(i, ': ', files[i], "\n", sep = "")
    }
    cat(i+1, ': ', '*Start over*', sep='')
    # Read user input
    selection <- as.integer(readline("Enter your selection: "))
    # Validate selection
    if (is.na(selection) || selection < 1 || selection > length(files)+1) {
      cat("Invalid selection. Please try again.\n")
    } else if (selection == length(files)+1) {
      connection <- 'data' 
    } else {
      # Keep history of previous selection
      connection_old <- connection
      # Set the connection as the selected file
      connection <- file.path(connection_old, files[selection])
    }
  }
  if (!file.info(connection)$isdir & connection %in% metadata$Connection) {
    cat('Connection already exists in metadata.csv.\n')
    dup_conn <- readline('Are you sure you want to create a new record for a duplicate connection? (y/n): ')
    if (tolower(dup_conn) != 'y') {
      connection <- connection_old
    }
  }
}
cat(glue::glue('Connection: {connection}\n'))

# All other cols ------------------------
# ask for other metadata values, print back values, and confirm with user that values are correct
for (col in colnames(metadata)[!colnames(metadata) %in% c('Product_ID', 'Connection')]) {
  if (exists('product_name')) {
    assign(tolower(col), get_value(col, product_name))
  } else {
    assign(tolower(col), get_value(col))
  }
}

# Create new row for df -----------------
# set product_id to NA for now (it will be filled in later along with any other NA values)
product_id <- NA
# create a one-row data frame and fill it with the previously set values
row <- data.frame(matrix(ncol=ncol(metadata), nrow=1))
names(row) <- names(metadata)
for (col in names(metadata)) {
  row[col] <- get(tolower(col))
}

head(row)

# Write new csv -------------------------
sendit <- readline('Write new line to metadata.csv? "y" to send it: ')

if (tolower(sendit)=='y') {
  # check to make sure formatting is the same between files
  metadata_new <- rbind(metadata, row)
  # fill in unids 
  na_id <- is.na(metadata_new$Product_ID)
  metadata_new$Product_ID[na_id] <- generate_unid(metadata_new$Product_ID[na_id], metadata_new$Product_ID)
  # write new file (should be the same besides 1 additional row) to metadata.csv
  write.csv(metadata_new, file='metadata.csv', row.names=FALSE, quote=FALSE, na='')
} else {
  cat('File was not written; no update to metadata.csv')
}

rm(product_name) # ensure product name is removed if user decided to rerun script w/o clearing env
