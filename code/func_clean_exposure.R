

func_clean_exposure <- function(data) {
  d <- data %>%
    dplyr::mutate(
      exposure_type = gsub("Other: ", "", exposure_type),
      exposure_type = gsub("\\s*\\([^\\)]+\\)", "", exposure_type), # remove text within parenthesis 
      
      ## text cleaning 
      exposure_type = gsub("L4 physical activity",       "L4 - physical activity in nature", exposure_type),
      exposure_type = gsub("L1 surrounding greenness",   "L1 - neighborhood/residential exposure", exposure_type),
      exposure_type = gsub("L2 objective accessibility", "L2 - objective accessibility", exposure_type),

      ## final formatting 
      exposure_type = gsub(",+", ",", exposure_type), ## Removing repeated punctuation character from a string
      exposure_type = gsub(";+", ";", exposure_type), ## Removing repeated punctuation character from a string
      exposure_type = trimws(exposure_type),
      exposure_type = str_squish(exposure_type)) %>%
    dplyr::filter(exposure_type != 'NA') %>%
    as.data.frame()
  
  return(d)
}