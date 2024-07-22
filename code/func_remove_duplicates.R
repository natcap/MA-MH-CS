## Function to remove duplicates within each cell
remove_duplicates <- function(text) {
  unique_elements <- str_split(text, ";")[[1]]
  unique_elements <- trimws(unique_elements)
  unique_elements <- unique(unique_elements)
  return(paste(unique_elements, collapse = ";"))
}