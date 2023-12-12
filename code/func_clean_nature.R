
#' Clean messy data in `nature_type`
#' @description Clean messy data
#'
#' @usage func_clean_nature_type(data, column_name, aggregate = F)
#' 
#' @param data          Data frame as the input
#' @param column_name   The target column for cleaning, in a string format
#' @param aggregate     Whether to aggregate the detailed nature landscapes; default is FALSE
#' 

## Function to remove duplicates within each cell
remove_duplicates <- function(text) {
  unique_elements <- str_split(text, ";")[[1]]
  unique_elements <- trimws(unique_elements)
  unique_elements <- unique(unique_elements)
  return(paste(unique_elements, collapse = ";"))
}



func_clean_nature_type <-  function(data, column_name, aggregate = F) {
  
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      !!column_name := gsub("Other\\: | \\- General", "", !!sym(column_name)),
      !!column_name := gsub("\\*", "", !!sym(column_name)),

      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name)),
    ) %>%
    as.data.frame()
  
  if (aggregate == F) {
    d <- d
    
  ## to aggregate the nature types
  } else {
    d <- d %>%
      dplyr::mutate(
        !!column_name := gsub("\\- Farmland|\\- Forest\\/Tree|\\- Garden|\\- Grassland|\\- Park|\\- Green alley/Roadside green", "", !!sym(column_name)), 
        !!column_name := gsub("\\- Green roof/wall|\\- Shrub\\/scrub", "", !!sym(column_name)),
        !!column_name := gsub("\\- Open water|\\- Beach\\/coastline|\\- Wetland", "", !!sym(column_name)),
        
        !!column_name := trimws(!!sym(column_name)),
        !!column_name := str_squish(!!sym(column_name)),
        
        ## to remove duplicates within each cell (e.g., multiple "Greenspace" in a cell)
        !!column_name := sapply(!!sym(column_name), remove_duplicates)
      ) %>%
      as.data.frame()
  }
  
  return(d)
}



func_clean_nature_quant <-  function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      !!column_name := gsub("Other\\: | \\- General", "", !!sym(column_name)),
      !!column_name := gsub("\\*", "", !!sym(column_name)),
      
      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name)),
    ) %>%
    as.data.frame()
  
  return(d)
}



func_clean_buffer <-  function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      
      ## clean `unit` format
      !!column_name := gsub('km', '', !!sym(column_name)),
      !!column_name := gsub('m',  '', !!sym(column_name)),

      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name)),
    ) %>%
    as.data.frame()
  return(d)
}



func_clean_bufferunit <-  function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      
      ## clean `unit` format
      !!column_name := gsub('Kilometers|\\(|\\)', 'km', !!sym(column_name)),
      !!column_name := gsub('Meters', 'm', !!sym(column_name)),
      
      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name)),
    ) %>%
    as.data.frame()
  return(d)
}
