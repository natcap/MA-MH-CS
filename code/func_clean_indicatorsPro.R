

func_clean_indicatorsPro <-  function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("Other\\: |in general\\*", "", !!sym(column_name)),
      !!column_name := gsub("\\(include schizophrenia\\)", "", !!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      !!column_name := gsub("\\/Restorativeness", "", !!sym(column_name)),
      !!column_name := gsub("Restoration experience|Restorative experience|Restorativeness", 
                            "Restorative effects", !!sym(column_name), ignore.case = T), 
      !!column_name := gsub("Attention capacity|Sustained attention", "Attention", !!sym(column_name), ignore.case = T), ##  
      !!column_name := gsub("Behavioural", "Behavioral", !!sym(column_name), ignore.case = T), ##  
      !!column_name := gsub("Behavioral problems", "Behavioral disorder", !!sym(column_name), ignore.case = T), ## 
      !!column_name := gsub("Burnout symptom", "Burnout", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Burnout fatigue|burnout fatigue", "Burnout", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Cognitions and Conduct", "Cognitive functioning", !!sym(column_name), ignore.case = T), # 
      !!column_name := gsub("Quality of life|Quality of Life", "Quality of life;", !!sym(column_name)),# 
      !!column_name := gsub("Psychological distress|psychological distress", "Mental distress", !!sym(column_name)),# # 
      !!column_name := gsub("psychiatric morbidity|psychiatric disorder|mental illness", "Mental disorder", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Symptoms of psychosis", "Psychosis", !!sym(column_name), ignore.case = T), #### 
      
      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))
    ) %>%
    as.data.frame()
  
  return(d)
}
