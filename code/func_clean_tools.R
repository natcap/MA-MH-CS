

func_clean_tools <- function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := gsub("Other: ", "", !!sym(column_name)),
      !!column_name := gsub("scales|Scales", "scale", !!sym(column_name), ignore.case = T),
      ## remove text within parenthesis 
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), 
      !!column_name := gsub(
        paste("Anti\\-depressant prescribing", "Antidepressant prescriptions", 
              "Antidepressant prescription rates", "Antidepressant use", sep = "|"),
        "Antidepressant Rx", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("CES\\-D\\-10|CESD\\-10", "CES\\-D10", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("CES\\-D\\-20|CESD\\-20", "CES\\-D20", !!sym(column_name), ignore.case = T),
      !!column_name := gsub(
        paste("Diagnosed", "Diagnoses", 
              "Diagnosis by professionals", 
              "Diagnosis of mental health disorders", sep = "|"), 
        "Diagnosis", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Digit Span Backward", "DSB", !!sym(column_name), ignore.case = F), ## 
      !!column_name := case_when(!!sym(column_name) %in% c('Digit', 'digit', 'Digit-span Task') ~ "Digit Span",
                       !!sym(column_name) == 'Digit test' ~ "Digit Span",
                       T ~ !!sym(column_name)),
      !!column_name := gsub("Electroencephalography", "EEG", !!sym(column_name), ignore.case = F), ## # 
      !!column_name := gsub("EQ-5D", "EQ5D", !!sym(column_name), ignore.case = F), ## # 
      !!column_name := gsub("K\\-10", "K10", !!sym(column_name), ignore.case = F),
      !!column_name := gsub("Life Satisfaction approach", 'Life satisfaction', !!sym(column_name), ignore.case = T),
      !!column_name := gsub(
        paste("Official\\/National mental health", 
              "National survey", 
              "Office for National Statistics", 
              "UK's Office of National Statistics",
              sep = "|"),
        'Official MH survey', !!sym(column_name), ignore.case = T),#
      !!column_name := gsub("survey survey", 'survey', !!sym(column_name), ignore.case = T),# 
      !!column_name := gsub("Stroop Test", 'Stroop test', !!sym(column_name), ignore.case = F),# 
      !!column_name := gsub("PNAS", 'PANAS', !!sym(column_name), ignore.case = T),# 
      !!column_name := gsub("Profile of mood state questionnaire", 'POMS', !!sym(column_name), ignore.case = T),# 
      !!column_name := gsub("Psychiatric disorders", 'Mental disorder', !!sym(column_name), ignore.case = T),# 
      !!column_name := gsub("Psychiatric disorder", 'Mental disorder', !!sym(column_name), ignore.case = T),
      !!column_name := gsub("PHQ-2|PHQ-4|PHQ-8|PHQ-9", 'PHQ', !!sym(column_name), ignore.case = T),
      !!column_name := gsub("RAND-36", 'RAND36', !!sym(column_name), ignore.case = T), # # 
      !!column_name := gsub("Restoration Outcome Scale|ROS\\-S", 'ROS', !!sym(column_name), ignore.case = T), # 
      !!column_name := gsub("RSE or RSES|RSES", 'RSE', !!sym(column_name), ignore.case = F), # 
      !!column_name := gsub("SCL90", 'SCL-90', !!sym(column_name), ignore.case = F), # 
      !!column_name := gsub("SWEMWBS|WEMWBS 7-item", 'WEMWBS', !!sym(column_name), ignore.case = F), # # 
      !!column_name := gsub(",+", ",", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := gsub(";+", ";", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name))
      
    ) %>% 
    # dplyr::filter(!!sym(column_name) != 'NA') %>%
    as.data.frame()
  
  return(d)
}

