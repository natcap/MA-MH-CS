

library(dplyr)
library(stringr)
library(conflicted)
conflict_prefer_all(winner = "dplyr", quiet = T)

func_clean_indicatorsPro <-  function(data, column_name) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := gsub("Other\\: |in general\\*", "", !!sym(column_name)),
      !!column_name := gsub("\\(include schizophrenia\\)", "", !!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      
      !!column_name := gsub("\\/Restorativeness", "", !!sym(column_name)),
      !!column_name := gsub("Restoration experience|Restorative experience|Restorativeness|Psychological Restorative Effects", 
                            "Restorative effects", !!sym(column_name), ignore.case = T), 
      !!column_name := gsub("Restoration", "Restorative effects", !!sym(column_name)),
      !!column_name := gsub("Attention capacity|Sustained attention|Attention Functioning", "Attention", !!sym(column_name), ignore.case = T), ##  
      !!column_name := gsub("Behavioural", "Behavioral", !!sym(column_name), ignore.case = T), ##  
      !!column_name := gsub("Behavioral problems", "Behavioral disorder", !!sym(column_name), ignore.case = T), ## 
      !!column_name := gsub("Burnout symptom", "Burnout", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Burnout fatigue|burnout fatigue", "Burnout", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Cognitions and Conduct", "Cognitive functioning", !!sym(column_name), ignore.case = T), # 
      !!column_name := gsub("Quality of life|Quality of Life|Recovering Quality Of Life", "Quality of life;", !!sym(column_name)),# 
      !!column_name := gsub("Psychological distress|psychological distress", "Mental distress", !!sym(column_name)),# # 
      !!column_name := gsub("psychiatric morbidity|psychiatric disorder|mental illness", "Mental disorder", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Symptoms of psychosis", "Psychosis", !!sym(column_name), ignore.case = T), #### 
      !!column_name := gsub("Subjective Vitality", "Vitality", !!sym(column_name), ignore.case = T),
      
      !!column_name := gsub(",", ";", !!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))
    ) %>%
    as.data.frame()
  
  return(d)
}





func_clean_indicator_level2 <-  function(data, column_name, upper_case = T) {
  d <- data %>%
    dplyr::mutate(
      
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      !!column_name := stringr::str_squish(!!sym(column_name)),
      
      !!column_name := gsub("–|\\/", "-", !!sym(column_name)),
      !!column_name := gsub("A\\-H|anger\\-hostility|anger and hostility", "Anger", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("hostility", "Anger", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Confused|confusion\\-bewilderment", "Confusion", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("depression-dejection|depression and dejection", "Depression", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("fatigue\\-inertia|Mental Fatigue", "Fatigue", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("tension\\-anxiety|tension and anxiety|Tension\\/Anxiety|T\\-A", "Tension Anxiety", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Tension Anxiety|Tension", "Anxiety", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("total mood disturbance|Total Mood of Disturbance", "TMD", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Mood Disturbance|Total POMS", "TMD", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("vigor\\-activity|Vigour", "Vigor", !!sym(column_name), ignore.case = T),
      
      !!column_name := ifelse(!!sym(column_name) == "C", "Confusion", !!sym(column_name)),
      !!column_name := ifelse(!!sym(column_name) == "D", "Depression", !!sym(column_name)),
      !!column_name := ifelse(!!sym(column_name) == "F", "Fatigue", !!sym(column_name)),
      !!column_name := ifelse(!!sym(column_name) == "V", "Vigor", !!sym(column_name)),
      
      !!column_name := gsub("Positive Affect", "Positive Affect", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Negative Affect", "Negative Affect", !!sym(column_name), ignore.case = T),
      
      !!column_name := trimws(!!sym(column_name))
    ) %>%
    as.data.frame()
  
  
  if(upper_case == T) {
    d <- d %>%
      dplyr::mutate(
        !!column_name := ifelse(
          !!sym(column_name) == toupper(!!sym(column_name)), !!sym(column_name),
          stringr::str_to_title(!!sym(column_name))
        )
      ) 
  } else {
    d <- d
  }
      
  
  return(d)
}

# exp_sub_mods_md2_test <- exp_sub_mods_md2 %>%
#   func_clean_indicator_sub(data = ., column_name = 'MH_indicator_o2')
# unique(exp_sub_mods_md2_test$MH_indicator_o2) %>% sort()

