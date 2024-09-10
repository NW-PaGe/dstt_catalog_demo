# Inserts ui and server code from src dir into index.qmd
# This is a workaround so that code can be stored outside of index.qmd and accessed by r-shinylive
# while being modular and DRY https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

# Read the index.qmd template as strings
index_qmd <- readLines("index.qmd")

# Read the contents of ui.R and server.R as strings
ui_code <- paste(readLines("src/ui.R"), collapse = "\n")
server_code <- paste(readLines("src/server.R"), collapse = "\n")

# Replace placeholders with the actual server/ui code
index_qmd <- gsub("source('src/ui.R') # insert ui_code here", ui_code, index_qmd, fixed=T)
index_qmd <- gsub("source('src/sever.R') # insert server_code here", server_code, index_qmd, fixed=T)

# Write the modified file back to index.qmd
writeLines(index_qmd, "index.qmd")
