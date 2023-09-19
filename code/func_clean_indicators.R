

func_clean_indicators <-  function(data) {
  d <- data %>%
    dplyr::mutate(
      Indicator = str_squish(Indicator),
      Indicator = gsub("Other\\: |in general\\*", "", Indicator),
      Indicator = gsub("\\(include schizophrenia\\)", "", Indicator),
      Indicator = gsub("\\s*\\([^\\)]+\\)", "", Indicator), # remove text within parenthesis 
      Indicator = gsub("\\/Restorativeness", "", Indicator),
      Indicator = gsub("Restoration experience|Restorative experience|Restorativeness", 
                       "Restorative effects", Indicator, ignore.case = T), 
      Indicator = gsub("Attention capacity|Sustained attention", "Attention", Indicator, ignore.case = T), ##  
      Indicator = gsub("Behavioural", "Behavioral", Indicator, ignore.case = T), ##  
      Indicator = gsub("Behavioral problems", "Behavioral disorder", Indicator, ignore.case = T), ## 
      Indicator = gsub("Burnout symptom", "Burnout", Indicator, ignore.case = T),
      Indicator = gsub("Burnout fatigue|burnout fatigue", "Burnout", Indicator, ignore.case = T),
      Indicator = gsub("Cognitions and Conduct", "Cognitive functioning", Indicator, ignore.case = T), # 
      Indicator = gsub("Quality of life|Quality of Life", "Quality of life;", Indicator),# 
      Indicator = gsub("Psychological distress|psychological distress", "Mental distress", Indicator),# # 
      Indicator = gsub("Symptoms of psychosis", "Psychosis", Indicator, ignore.case = T), #### 
      
      Indicator = gsub(",", ";", Indicator),
      Indicator = trimws(Indicator)
    ) %>%
    as.data.frame()
  
  return(d)
}
