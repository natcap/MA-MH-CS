

func_clean_exposure <- function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := gsub("Other: ", "", !!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      
      ## text cleaning 
      !!column_name := gsub("L4 physical activity",       "L4 - physical activity in nature", !!sym(column_name)),
      !!column_name := gsub("L1 surrounding greenness",   "L1 - neighborhood/residential exposure", !!sym(column_name)),
      !!column_name := gsub("L2 objective accessibility", "L2 - objective accessibility", !!sym(column_name)),

      ## final formatting 
      !!column_name := gsub(",+", ",", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := gsub(";+", ";", !!sym(column_name)), ## Removing repeated punctuation character from a string
      
      !!column_name := case_when(
        # str_detect(pattern = ";", string = !!sym(column_name)) ~ 'Mixed',
        !!sym(column_name) %in% c('L5 - gardening', 'L5 - gardening frequency') ~ 'L5 - nature participatory activities',
        TRUE ~ !!sym(column_name)),
      # !!column_name := gsub('L1 - |L3 - |L4 - |L5 - ', '', !!sym(column_name)), 
      
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := str_to_sentence(!!sym(column_name))
      ) %>%
    # dplyr::filter(!!column_name != 'NA') %>%
    as.data.frame()
  
  return(d)
}