docx_to_html <- function(doc) {
  #' Convert docx to html 
  #' 
  #' Return a semi formatted html version of a docx file. This function
  #' accepts docx objects (output by officer::read_docx) and outputs an html-marked character element
  #' with some headers and tables formatted. Media will not be included in output.
  #' 
  #' @param doc docx object output from officer::read_docx
  #' @seealso [officer::read_docx()]
  #' 
  
  
  library(officer)
  library(dplyr)
  library(htmltools)
  
  doc_content <- officer::docx_summary(doc) # convert doc to data frame with rows of docx elements
  
  doc_content <- dplyr::arrange(doc_content, doc_index, row_id, cell_id) # arrange df by index, then row id, then cell id
  
  html_content <- "" # Initialize html content element
  
  is_table <- FALSE # Set flag to check if a table is being constructed
  
  # Step through the document content rowwise to render paragraphs, headers, tables, and page breaks
  for (i in 1:nrow(doc_content)) {
    # close the row and table if the previous row was part of a table
    if (is_table & !doc_content$content_type[i] %in% "table cell") {
      html_content <- paste0(html_content, "</tr></table>")  # Close row and table
      is_table <- FALSE # reset to false
    }
    # if row is paragraph type ...
    if (doc_content$content_type[i] %in% "paragraph") {
      # insert a horizontal line if a page break is present
      if (tolower(doc_content$text[i]) %in% 'page break') {
        html_content <- paste0(html_content, htmltools::hr())
      # otherwise, if a heading style, insert text with a heading tag (add +1 to heading # as we assign title as h1 later)  
      } else if (grepl("heading [1-5]", doc_content$style_name[i])) {
        header = gsub("heading ([1-5])", "\\1", doc_content$style_name[i])
        html_content <- paste0(
          html_content,
          switch(as.numeric(header)+1,
                 htmltools::h1(doc_content$text[i]),
                 htmltools::h2(doc_content$text[i]),
                 htmltools::h3(doc_content$text[i]),
                 htmltools::h4(doc_content$text[i]),
                 htmltools::h5(doc_content$text[i]),
                 htmltools::h6(doc_content$text[i])
          )
        )
      # otherwise, if a title style, insert text with h1 tags
      } else if (tolower(doc_content$style_name[i]) %in% "title") {
        html_content <- paste0(html_content, htmltools::h1(doc_content$text[i]))
      # otherwise, insert text with paragraph tags
      } else {
        html_content <- paste0(html_content, htmltools::p(doc_content$text[i]))
      }
    # otherwise (if not a paragraph type), if it is a table cell in row 1 cell 1 (meaning, the first cell of a table), insert the table border
    } else if (doc_content$content_type[i] %in% "table cell" & doc_content$row_id[i]%in%1 & doc_content$cell_id[i]%in%1) {
      html_content <- paste0(html_content, "<table border='1'>")  # Start a table
      is_table <- TRUE # set table flag to true
    # otherwise, if table cell and the first cell of a new row, add a new row tag
    } else if (doc_content$content_type[i] %in% "table cell" & doc_content$row_id[i] > doc_content$row_id[i-1]) {
      html_content <- paste0(html_content, "</tr><tr>")  # Close prev row and start a new row
    # insert text if not paragraph of table cell w/o formatting
    } else if (!doc_content$content_type[i] %in% c("paragraph", "table cell")) {
      html_content <- paste0(html_content, doc_content$text[i])
    }
    # if a table cell, insert text with td tag
    if (doc_content$content_type[i] %in% "table cell") {
      html_content <- paste0(html_content, "<td>", doc_content$text[i], "</td>")  # Add cell content
    }
  }        
  
  # convert character element to element marked as HTML
  HTML(html_content)
}
