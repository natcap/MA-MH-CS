

func_clean_tools <- function(data) {
  d <- data %>%
    dplyr::mutate(
      Tool = gsub("Other: ", "", Tool),
      Tool = gsub("scales|Scales", "scale", Tool, ignore.case = T),
      Tool = gsub("\\s*\\([^\\)]+\\)", "", Tool), # remove text within parenthesis 
      Tool = gsub(
        paste("Anti\\-depressant prescribing", "Antidepressant prescriptions", 
              "Antidepressant prescription rates", "Antidepressant use", sep = "|"),
        "Antidepressant prescription", Tool, ignore.case = T),
      Tool = gsub("CES\\-D\\-10|CESD\\-10", "CES\\-D10", Tool, ignore.case = T),
      Tool = gsub(
        paste("Diagnosed", "Diagnoses", "Diagnosis by professionals", 
              "Diagnosis of mental health disorders", sep = "|"), 
        "Diagnosis", Tool, ignore.case = T),
      Tool = gsub("Digit Span Backward", "DSB", Tool, ignore.case = F), ## 
      Tool = case_when(Tool %in% c('Digit', 'digit', 'Digit-span Task') ~ "Digit Span",
                       Tool == 'Digit test' ~ "Digit Span",
                       T ~ Tool),
      Tool = gsub("Electroencephalography", "EEG", Tool, ignore.case = F), ## # 
      Tool = gsub("EQ-5D", "EQ5D", Tool, ignore.case = F), ## # 
      Tool = gsub("K\\-10", "K10", Tool, ignore.case = F),
      Tool = gsub("Life Satisfaction approach", 'Life satisfaction approach', Tool, ignore.case = T),
      Tool = gsub(
        paste("Official/National mental health", "National survey", "Office for National Statistics", 
              "UK's Office of National Statistics",
              sep = "|"),
        'Official MH survey', Tool, ignore.case = T),#
      Tool = gsub("survey survey", 'survey', Tool, ignore.case = T),# 
      Tool = gsub("PNAS", 'PANAS', Tool, ignore.case = T),# 
      Tool = gsub("Profile of mood state questionnaire", 'POMS', Tool, ignore.case = T),# 
      Tool = gsub("Psychiatric disorders", 'Mental disorder', Tool, ignore.case = T),# 
      Tool = gsub("Psychiatric disorder", 'Mental disorder', Tool, ignore.case = T),
      Tool = gsub("PHQ-2|PHQ-4|PHQ-8|PHQ-9", 'PHQ', Tool, ignore.case = T),
      Tool = gsub("RAND-36", 'RAND36', Tool, ignore.case = T), # # 
      Tool = gsub("Restoration Outcome Scale", 'ROS', Tool, ignore.case = T), # 
      Tool = gsub("RSE or RSES|RSES", 'RSE', Tool, ignore.case = F), # 
      Tool = gsub("SCL90", 'SCL-90', Tool, ignore.case = F), # 
      Tool = gsub("SWEMWBS|WEMWBS 7-item", 'WEMWBS', Tool, ignore.case = F), # # 
      Tool = gsub(",+", ",", Tool), ## Removing repeated punctuation character from a string
      Tool = gsub(";+", ";", Tool), ## Removing repeated punctuation character from a string
      Tool = trimws(Tool),
      Tool = str_squish(Tool)
      
    ) %>% ## remove any strings in a ()
    dplyr::filter(Tool != 'NA') %>%
    as.data.frame()
  
  return(d)
}