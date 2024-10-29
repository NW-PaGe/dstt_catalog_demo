### write_metadata.R
### This script can be sourced on your local machine to interactively write a new metadata row to metadata.csv
### Working directory should be set to head of cloned repo (dstt_catalog_demo/)

# Read in current metadata file
metadata <- read.csv('metadata.csv')

# import unid assigner
source('src/generate_unid.R')

# create function to ask for metadata inputs and validate input
get_value <- function(col) {
  valid <- 'n'
  while (tolower(valid) != 'y') {
    # get set of example values to print
    example_vals <- unique(metadata[col])
    example_vals <- example_vals[!is.na(example_vals) & example_vals != '']
    example_vals <- example_vals[1:min(c(5, length(example_vals)))]
    example_vals <- paste(example_vals, collapse = ', ')
    if (nchar(example_vals) > 80) {
      example_vals <- paste0(substr(example_vals, 1, 80), '...')
    }
    # ask for column value
    prompt <- paste0('Enter ', col, ': (e.g., ', example_vals, ') ')
    col_value <- readline(prompt)
    # print typed value and confirm
    cat(col, ': ', col_value, '\n', sep='')
    cat('y: continue\n')
    cat('n: re-enter value\n')
    valid <- readline('Continue? y/n ')
  }
  col_value
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
    selection <- as.integer(readline(prompt = "Enter your selection: "))
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
}
cat(glue::glue('Connection: {connection}'))

# All other cols ------------------------
# ask for other metadata values, print back values, and confirm with user that values are correct
for (col in colnames(metadata)[!colnames(metadata) %in% c('Product_ID', 'Connection')]) {
  assign(tolower(col), get_value(col))
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