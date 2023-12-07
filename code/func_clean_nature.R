

func_clean_nature_type <-  function(data, column_name) {
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
