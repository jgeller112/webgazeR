# Switch Qmd code cell syntax ----

# Obtain a list of input files separated by `\n`
project_input <- Sys.getenv("QUARTO_PROJECT_INPUT_FILES")

# Convert the list into a new vector
project_input_files <- unlist(strsplit(project_input, split = "\n"))

# Use grep to find files with the .qmd extension
qmd_files <- grep("\\.qmd$", project_input_files, value = TRUE)

# Convert from Rmd style/ Historical chunk option syntax to Quarto's Hashpipe YAML syntax
for (qmd in qmd_files) {
  knitr::convert_chunk_header(qmd, output = qmd, type = "yaml")
}

# Add panelize tag for converting from R to webR a code cell ----

# Function to wrap R code chunks in an RMarkdown file with ::: {.to-panel}
wrap_r_chunks_with_panel <- function(file_path) {
  # Read the file content
  lines <- readLines(file_path)
  
  # Determine the number of additional lines needed
  additional_lines <- sum(grepl("^```\\{r.*\\}$", lines)) * 2
  
  # Create a vector to hold the modified lines
  modified_lines <- character(length(lines) + additional_lines)
  
  # Initialize variables to store the current position and chunk state
  current_position <- 1
  in_chunk <- FALSE
  
  for (line in lines) {
    if (grepl("^```\\{r.*\\}$", line) && !in_chunk) {
      # Start of R code chunk
      modified_lines[current_position] <- "::: {.to-webr}"
      modified_lines[current_position + 1] <- line
      current_position <- current_position + 2
      in_chunk <- TRUE
    } else if (grepl("^```$", line) && in_chunk) {
      # End of R code chunk
      modified_lines[current_position] <- line
      modified_lines[current_position + 1] <- ":::"
      current_position <- current_position + 2
      in_chunk <- FALSE
    } else {
      # Regular line
      modified_lines[current_position] <- line
      current_position <- current_position + 1
    }
  }
  
  # Write the modified content to the output file
  writeLines(modified_lines, file_path)
}

for (qmd in qmd_files) {
  wrap_r_chunks_with_panel(qmd)
}
